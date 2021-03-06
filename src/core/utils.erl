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
%% @doc Generic utilities used across the various modules
%%
%% @end
-module(utils).

-export([
	valid_pidname/1,
	invalid_pidnames/1,
	valid_eptype/1,
	valid_eptype/2,
	invalid_eps/1,
	valid_ep/2,
	valid_address/2,
	apply_attribute_filter_to_list/2,
	apply_attribute_filter_to_list/3,
	apply_attribute_filter/2,
	apply_attribute_filter/3,
	apply_attribute_filter/4,
	load_defaults/3,
	load_file/3,
	get_param/1,
	appname/0,
	commonvalidate/3,
	weak_validate_syntax/1,
	field_present/3,
	errors_toEJSON/1,
	check_fields/1,
	invalid_pidnames_asError/1,
	invalid_eps_asError/1
	]).	
	
-include("e_alto.hrl").
-define(PIDRE, "^[a-zA-Z0-9\-:_@.]{1,64}$").
	
err_to_string(Atom) ->
	case Atom of
		?E_SYNTAX -> <<"E_SYNTAX">>; 
		?E_MISSING_FIELD -> <<"E_MISSING_FIELD">>;
		?E_INVALID_FIELD_TYPE -> <<"E_INVALIDE_FIELD_TYPE">>;
		?E_INVALID_FIELD_VALUE -> <<"E_INVALID_FIELD_VALUE">>;
		?ALTO_ERR -> <<"ALTO_ERROR">>;
		_ -> list_to_binary(atom_to_list(Atom))
	end.
	
errors_toEJSON(Errors) when is_list(Errors)->
	_Attrs = lists:foldl(fun({?ALTO_ERR, Code, SyntaxError}, AccIn) -> 
		 [ { <<"code">>, err_to_string(Code) }, { <<"syntax-error">>, SyntaxError } ] ++ AccIn
		end, [], Errors),
	{ struct, [{ <<"meta">>, [ { struct, _Attrs } ] }] };
errors_toEJSON(Errors) ->
	errors_toEJSON([Errors]).
	
check_fields(CheckList) when is_list(CheckList) -> 
		lists:foldl(fun({EJPath,EJStruct,ErrMsg},AccIn) -> 
			 case field_present(EJPath,EJStruct,ErrMsg) of
				true -> AccIn;
				Value -> [Value] ++ AccIn 
			 end
			end, [], CheckList).
	
field_present(EJPath, EJStructure, ErrorString) ->
	case (ej:get(EJPath,EJStructure) =/= undefined) of
		true -> true;
		false -> {?ALTO_ERR, ?E_MISSING_FIELD, ErrorString}
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
	%Validate against ALTO Schema... Someday...
	try 
	  ParsedBody = mochijson2:decode(Body),
	  lager:info("Request is valid JSON - Passes Weak Validation Test",[]),
	  {ok, ParsedBody}
	catch 
		_:_ ->
			lager:info("Invalid JSON Found",[]),
			{?ALTO_ERR, ?E_SYNTAX, <<"Invalid JSON Found">>}
	end.	
	
appname() -> ?APPLICATIONNAME.

invalid_pidnames_asError(List) when is_list(List) ->
	case length(List) of 
		0 -> nothing;
		_ -> 
		 _Items = lists:foldl(fun(E,Acc) -> [binary_to_list(E)] ++ Acc end, [], List),
		 _Errors = list_to_binary( string:join(_Items, ",") ),
		 {?ALTO_ERR, ?E_INVALID_FIELD_TYPE, << <<"Pid Names are not Valid - [">>/binary, _Errors/binary, <<"]">>/binary >> }
	end.

invalid_pidnames(undefined) ->
	[];
invalid_pidnames(List) when is_list(List) ->
	{ok,RE} = re:compile(?PIDRE),
	invalid_pidnames1(List,RE,[]).
	
invalid_pidnames1([],_,AccIn) ->
	AccIn;
invalid_pidnames1([H|T],RE,AccIn) ->
	Val = case is_binary(H) of
		true -> binary_to_list(H);
		false -> H
	end,
	case re:run(Val,RE) of
		{match, _} -> invalid_pidnames1(T,RE,AccIn);
		nomatch -> invalid_pidnames1(T,RE,[H] ++ AccIn)
	end.
	
valid_pidname(String) when is_binary(String) ->
	valid_pidname(binary_to_list(String));
valid_pidname(String) when is_list(String) ->
	{ok,RE} = re:compile(?PIDRE),
	re:run(String,RE).

get_param(ParamName) ->
	case application:get_env(?APPLICATIONNAME, ParamName) of
		{ok, _Result} -> 
			lager:info("~p value found = ~p", [atom_to_list(ParamName),_Result]),
			_Result;
		Else ->
			lager:info("Parameter is not present - Received ~p",[Else]),
			undefined
	end.

load_defaults(Identifier, FilePath, LoadFunction) ->
	lager:info("~p--Load ~p Defaults --Starting Load",[?MODULE, Identifier]),
	_List = load_files( get_param(FilePath), LoadFunction, []),
	lager:info("~p--Load ~p Defaults--Completed",[?MODULE, Identifier]),
	_List.	

addTo(Val,AccIn) ->
	case Val of 
	 {Path,_,X} ->  [{Path,X}]++AccIn;
	 {S1,S2} -> [{S1,S2}] ++ AccIn
	end.

load_files([],_,AccIn) ->
	AccIn;
load_files([{Path,FileLoc}|T],LoadFunction,AccIn) ->
	load_files(T,LoadFunction, addTo( load_file(LoadFunction,Path,FileLoc), AccIn ));
load_files({_,[]},_,AccIn) ->
	AccIn;
load_files(undefined,_,[]) ->
	[];
load_files({Path,[H|T]=FileLocs}, LoadFunction, AccIn) when is_list(FileLocs) ->
	load_files({Path,T}, LoadFunction, addTo( load_file(LoadFunction,Path,H), AccIn ));
load_files({Path,FileLoc}, LoadFunction, _) ->
	{Path,_,X} = load_file(LoadFunction,Path,FileLoc),
	[{Path,X}].

load_file(LoadFunction,Path,FileLoc) ->
	lager:info("~p--Load File-- ~p -- Beginning File Read at location ~p",[?MODULE,Path,FileLoc]),	
	case file:read_file(FileLoc) of
		{ok, _File} ->
			lager:info("~p--Load File-- Read complete - Starting Storage",[?MODULE]),	
			case LoadFunction(Path,FileLoc,_File) of
				{ok, _ResourceId, X} ->
					lager:info("~p--Load File-- Completed",[?MODULE]),
					lager:info("Loaded Content -> ~n~n~p~n~n~n~n~n~n",[X]),
					{Path, _ResourceId, X};
				{?ALTO_ERR, _, _} ->
					{Path, error}
			end;				
		{error, Value} ->
			lager:info("An error occurred reading the file - ~p",[Value]),
			{Path, error}
	end.	
	
invalid_eps_asError(List) when is_list(List) ->
	case length(List) of 
		0 -> nothing;
		_ -> 
		 _Items = lists:foldl(fun(E,Acc) -> [binary_to_list(E)] ++ Acc end, [], List),
		 _Errors = list_to_binary( string:join(_Items, ",") ),
		 {?ALTO_ERR, ?E_INVALID_FIELD_TYPE, << <<"Endpoint Names are not Valid - [">>/binary, _Errors/binary, <<"]">>/binary >> }
	end.	
	
invalid_eps(undefined) ->
	[];
invalid_eps(List) when is_list(List) ->
	lists:foldl(fun(E,AccIn) -> 
				 case valid_ep(E,nothing) of
					true -> AccIn;
					false -> [E] ++ AccIn
				 end
				end,
				[],
				List).
	
valid_ep(EPAddress,_) when is_binary(EPAddress) ->
	valid_ep(binary_to_list(EPAddress), nothing);
valid_ep(EPAddress,_) when is_list(EPAddress) ->
	_Pos = string:chr(EPAddress,$:),
	Type = string:sub_string(EPAddress,1,_Pos-1),
	Value = string:sub_string(EPAddress,_Pos+1),
	case Type of 
		"ipv4" -> valid_address(Value,ipv4);
		"ipv6" -> valid_address(Value,ipv6);
		_ -> false
	end.	
	
valid_address(Address,Type) when is_binary(Address) ->
	valid_address(binary_to_list(Address),Type);
valid_address(Address,Type) ->
	_Type = case inet:parse_address(Address) of
		{ok, FinalAddress} ->
			case size(FinalAddress) of
				4 -> ipv4;
				8 -> ipv6;
				_ -> nothing
			end;
		_ ->
			nothing
	end,
	_Type == Type.
	
valid_eptype(EPAddress, _) ->
	valid_eptype(EPAddress).
	
valid_eptype(EPAddress) when is_binary(EPAddress) ->
	[Type,_] = binary:split(EPAddress,[<<":">>]),
	case Type of 
		<<"ipv4">> -> true;
		<<"ipv6">> -> true;
		_ -> false
	end;
valid_eptype(EPAddress) when is_list(EPAddress) ->
	[Type,_] = string:tokens(EPAddress,":"),
	case Type of 
		"ipv4" -> true;
		"ipv6" -> true;
		_ -> false
	end.	
	
%%
%% @doc Applies an AttributeFilter to a list of JSONObjects
%% @end
%%
apply_attribute_filter_to_list(JSONObjectList, undefined) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, []) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, AttributeFilter) when is_list(AttributeFilter) ->
	apply_attribute_filter_to_list1(JSONObjectList, lists:usort(AttributeFilter), []).

%%
%% @doc Applies an AttributeFilter to a list of JSONObjects
%% @end
%%
apply_attribute_filter_to_list(JSONObjectList, undefined, _) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, [], _) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, AttributeFilter, CleanIt) when is_list(AttributeFilter) ->
	List = apply_attribute_filter_to_list1(JSONObjectList, lists:usort(AttributeFilter), []),
	case CleanIt of 
		false -> List;
		true -> clean_list(List)
	end.

%%
%% List iteration that applies apply_attribute_filter to each element
%%
apply_attribute_filter_to_list1([], _, AccIn) ->
	AccIn;
apply_attribute_filter_to_list1([H|T], AttributeFilter, AccIn) ->
	apply_attribute_filter_to_list1(T, AttributeFilter, [apply_attribute_filter1(AttributeFilter, H)] ++ AccIn).
		
%%
%% @doc Applies an AttributeFilter to a JSON Object, e.g. { ... },or named 
%% object - "somename" : { ... }
%% @end
%%		
apply_attribute_filter(undefined, JSONObject) ->		
	JSONObject; %% undefined filter
apply_attribute_filter([], JSONObject) ->
	JSONObject; %% empty filter
apply_attribute_filter(Filter, JSONObject) ->
	apply_attribute_filter1(lists:usort(Filter),JSONObject).
	
%% Internal Function	
apply_attribute_filter1(Filter, {struct,_}=JSONObject) ->
	apply_attribute_filter1(Filter, [], JSONObject, []);  
apply_attribute_filter1(Filter, JSONObject) when is_list(JSONObject) ->
	{struct, apply_attribute_filter1(Filter, [], JSONObject, []) };  
apply_attribute_filter1(Filter, {ObjectName,Value}) when is_list(Value) -> 
	%% Object is a named object so add that to the path in order to apply the filter
	{ObjectName, apply_attribute_filter1(Filter, [], Value, [])};
apply_attribute_filter1(Filter, {ObjectName,{struct,_}=Value}) -> 
	%% Object is a named object so add that to the path in order to apply the filter
	{ObjectName, {struct, apply_attribute_filter1(Filter, [], Value, [])}}.

%%
%% @doc Removes empty objects in a list after the filter is applied.
%%
clean_list(FilteredList) when is_list(FilteredList)->
	lists:foldl(fun(E,AccIn) -> 
					case should_clean(E) of
						true -> AccIn;
						false -> [E] ++ AccIn
					end
				end,
				[],
				FilteredList).

should_clean({struct,[]}) ->
	true;  
should_clean({_,[]}) -> 
	true;
should_clean({_,{struct,[]}}) -> 
	true;
should_clean(_) ->
	false.

%%
%% @doc Applies an attribute filter to a JSON Document using the 
%% Base Path to prepend to the filter when searching
%% @end
%%
apply_attribute_filter(Filter, BasePath, JSONObject) ->
	apply_attribute_filter1(lists:usort(Filter), BasePath, JSONObject, []).

apply_attribute_filter(undefined, _, JSONObject,_) ->
	JSONObject;
apply_attribute_filter([], _, JSONObject,_) ->
	JSONObject;
apply_attribute_filter(Filter, BasePath, JSONObject,EmptyListIfClean) ->
	Val = apply_attribute_filter1(lists:usort(Filter), BasePath, JSONObject, []),
	case (EmptyListIfClean andalso should_clean(Val)) of
		false -> Val;
		true -> []
	end.
	
%%
%% Internal function that iterates over the provided FilterList
%% an applies property filters.
%%
apply_attribute_filter1([], _, _, AccIn) ->
	AccIn;
apply_attribute_filter1([H|T], BasePath, JSONObject, AccIn) ->
	%% Assert autofill attributes ...
	case ej:get(BasePath ++ [H],JSONObject) of
		undefined -> apply_attribute_filter1(T, BasePath, JSONObject, AccIn);
		Value -> apply_attribute_filter1(T, BasePath, JSONObject, [{H,Value}] ++ AccIn )
	end.
