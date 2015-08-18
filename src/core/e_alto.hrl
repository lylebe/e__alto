% License: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%% @author Lyle Bertz <lyleb551144@gmail.com>
%% @copyright Copyright 2015 Lyle Bertz
%%
%% This is a generic header file for common macros and defaults.
%% 

-compile([{parse_transform, lager_transform}]).

-define(APPLICATIONNAME, e_alto).
-define(ALTOSCHEMAKEY, schema).
-define(JESSE_ETS, jesse_ets).

-record(costmetric, { name, mode, metric, description=undefined }). 
-record(resourceentry, { name, type, uri, mediatype, accepts=undefined, capabilities=undefined, uses=[] }).

%%
%% Internal method to determine if the application is loaded.
%%
-spec is_app_loaded() -> boolean().

is_app_loaded() ->
	lists:keymember(?APPLICATIONNAME, 1, application:loaded_applications()).
	
%%
%% Loads the application
%%	
-spec load_application() -> already_loaded | ok | {error, any()}.

load_application() ->
	load_application([silent]).

%%
%% Loads application with Options.
%% 
%% Options
%% - silent - merely return ok in all successful use cases rather
%% 	 than being verbose about the state of the application.
%%
-spec load_application(Options :: [any()]) -> term() | {error, any()}.

load_application(Options) ->
	application:load(lager),
	lager:start(),
	lager:set_loglevel(lager_console_backend, debug),
	application:load(uuid),
	case is_app_loaded() of
		false -> 
			lager:info("Application was not loaded - loading it as ~p", [ atom_to_list(?APPLICATIONNAME) ]),
			application:load(?APPLICATIONNAME);
		true -> 
			lager:info("Application was loaded - moving on",[]),
			case lists:keymember(silent,1,Options) of
				false -> already_loaded;
				true -> ok
			end
	end,
	application:ensure_all_started(e_alto).

%%
%% Schema file loading
%%
-spec load_schema() -> ok | any().

load_schema() ->
	_SchemaFile = application:get_env(?APPLICATIONNAME, schema, ""),
	lager:info("e_alto_updates:init - Schema file = ~p", [_SchemaFile]),
	{ok, _File} = file:read_file(_SchemaFile),
	SchemaStruct = mochijson2:decode( binary_to_list(_File) ),
	jesse:add_schema(?ALTOSCHEMAKEY, SchemaStruct),
	lager:info("Schema File Load Complete and stored under key ~p", [ ?ALTOSCHEMAKEY ]),
	ok.	
	
is_schema_loaded() ->
	% Hack for jesse to ensure it won't blow up if the schema is not 
	% loaded or the table is not created.
	case ets_table_exists(?JESSE_ETS) of
		false -> false;
		true ->
			%Now try to read from the database
			try 
				jesse_database:read(?ALTOSCHEMAKEY),
				true
			catch 
				error:badarg ->
					false;
				{database_error,?ALTOSCHEMAKEY,schema_not_found} ->
					false
			end
	end.
	
commonvalidate(JSON,TypeName,SyntaxValidationFunction) ->
	case weak_validate_syntax(JSON) of
		{ok, Body} -> 
			lager:info("~p passed weak validation test",[TypeName]),
			_Res = SyntaxValidationFunction(Body),
			lager:info("Will return ~p for syntax validation",[_Res]),
			_Res;
		SomethingElse -> 
			lager:info("~p did not pass weak validation check",[TypeName]),
			SomethingElse
	end.

weak_validate_syntax(Body) when is_binary(Body) ->
	weak_validate_syntax( binary_to_list(Body) );
weak_validate_syntax(Body) when is_list(Body) ->
	% STEP - Validate against ALTO Schema
	try 
	  ParsedBody = mochijson2:decode(Body),
	  lager:info("Request is valid JSON - Passes Weak Validation Test",[]),
	  {ok, ParsedBody}
	catch 
		error ->
			lager:info("Invalid JSON Found",[]),
			{error, 422, "422-1 Operation result create invalid JSON"}
	end.
	
validate_syntax(Body) when is_binary(Body) ->
	validate_syntax( binary_to_list(Body) );
validate_syntax(Body) when is_list(Body) ->
	% STEP - Validate against ALTO Schema
	try 
	  ParsedBody = mochijson2:decode(Body),
	  case is_schema_loaded() of
		true -> ok;
		false -> load_schema()
	  end,
	  lager:info("Schema Loaded - Validating Now",[]),
	  case jesse:validate(?ALTOSCHEMAKEY, ParsedBody) of
		{ok, JsonStruct} ->
				% STEP - Semantic Validation based on application type
				% also builds the new application data for the ALTO server
				{ok, Body};
		_ ->
			{error, 422, "422-2 Operation result created JSON that could not pass ALTO Schema check"}
	  end
	catch 
		error ->
			{error, 422, "422-1 Operation result create invalid JSON"}
	end.
	
%%
%% Determines if the table in question exists in ETS
%%
ets_table_exists(TableName) ->
  case ets:info(TableName) of
    undefined -> false;
    _ -> true
end.

updateResource(ResourceId, ApplicationType, Body, ApplicationState) ->
	lager:info("Making a Store attempt",[]),
	e_alto_backend:store(ResourceId, {ApplicationType,Body,ApplicationState}).
		
updateResource(ResourceId, Vtag, ApplicationType, Body, ApplicationState) ->
	lager:info("Making a Store attempt",[]),
	e_alto_backend:store(ResourceId, Vtag, {ApplicationType,Body,ApplicationState}).

getIRD() ->
	case registry:get_resource(<<"IRD">>) of
		not_found -> %Initialize IRD;
			updateIRD( {struct,[{<<"meta">>,{struct,[{<<"cost-types">>,{struct,[]}}] }},{<<"resources">>,{struct,[]}} ]} ),
			getIRD();
		Value -> Value
	end.

updateIRD(IRD) ->
	e_alto_backend:store(<<"IRD">>,{directory, IRD, undefined}).

in_ird(Name) ->
	IRD = getIRD(),
	case ej:get({"resources",Name},IRD) of
		undefined -> false;
		_ -> true
	end.

gen_path() ->
   lists:flatten(io_lib:format("~p~p~p", tuple_to_list(erlang:now()) )).

get_param(ParamName) ->
	case application:get_env(?APPLICATIONNAME, ParamName) of
		{ok, _Result} -> 
			lager:info("~p value found = ~p", [atom_to_list(ParamName),_Result]),
			_Result;
		Else ->
			lager:info("Parameter is not present - Received ~p",[Else]),
			undefined
	end.
