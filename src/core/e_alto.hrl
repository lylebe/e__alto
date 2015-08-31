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
	
updateIRD(IRD) ->
	e_alto_backend:store(<<"IRD">>,{directory, IRD, undefined}).	

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

get_param(ParamName) ->
	case application:get_env(?APPLICATIONNAME, ParamName) of
		{ok, _Result} -> 
			lager:info("~p value found = ~p", [atom_to_list(ParamName),_Result]),
			_Result;
		Else ->
			lager:info("Parameter is not present - Received ~p",[Else]),
			undefined
	end.
