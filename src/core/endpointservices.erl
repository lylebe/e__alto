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
%% @doc ALTO (RFC 7285) Endpoint Services functions
%% Endpoints are grouped together by a path.
%%
%% @end
-module(endpointservices).

-export([
	load_defaults/0,
	load_endpoints/1
	]).
	
-define(EPDEFPATHID, epdefpath).	
-define(EPDEFFILES, epfiles).
	
-include("e_alto.hrl").
	
load_defaults() ->
	lager:info("~p--Load EndpointServices Defaults --Starting Load",[?MODULE]),
	load_endpoints( get_param(?EPDEFFILES) ),
	set_default_epservice( get_param(?EPDEFPATHID) ),
	lager:info("~p--Load EndpointServices Defaults--Completed",[?MODULE]).
	
load_endpoints(undefined) ->
	ok;
load_endpoints({Path,[H|T]=FileLocs}) when is_list(FileLocs) ->
	load_endpoint_file(Path,H),
	load_endpoints({Path,T});
load_endpoints({Path,FileLoc}) ->
	load_endpoint_file(Path,FileLoc).

load_endpoint_file(Path,FileLoc) ->
	lager:info("~p--Loading Endpoints File -- ~p -- Beginning File Read at location ~p",[?MODULE,Path,FileLoc]),	
	case file:read_file(FileLoc) of
		{ok, _File} ->
			lager:info("~p--Load Endpoints File -- Read complete - Starting Storage",[?MODULE]),	
			{ok, _ResourceId, X} = store_endpoints( string:sub_string(Path,2),FileLoc,_File),
			lager:info("~p--Load Endpoints File -- Completed",[?MODULE]),
			lager:info("Loaded Endpoints -> ~n~n~p~n~n~n~n~n~n",[X]),
			{Path, X};
		{error, Value} ->
			lager:info("An error occurred reading the file - ~p",[Value]),
			{Path, error}
	end.	

set_default_epservice(Path) ->
	e_alto_backend:set_constant(?EPDEFPATHID, Path).
	
get_default_epservice(Path) ->
	e_alto_backend:get_constant(?EPDEFPATHID).
	
ep_query(Path,Body)	->
	case validate_epquery of 
		{ok, JSON} ->
			get_eps(Path, ej:get({"endpoints"},JSON), ej:get({"properties"},JSON));
		SomethingElse -> 
			SomethingElse
	end.
	
validate_epquery(JSON) ->
		case weak_validate_syntax(JSON) of
		{ok, Body} -> 
			lager:info("Map passed weak validation test",[]),
			case (ej:get({"properties"},Body) =/= undefined) andalso (ej:get({"endpoints"},Body) =/= undefined) of
				false ->
					lager:info("Missing attributes in request",[]),
					{error};
				true ->
					lager:info("Will return ~p for syntax validation",[Body]),
					{ok, Body}
			end;
		SomethingElse -> 
			lager:info("Did not pass weak validation check",[]),
			SomethingElse
	end.	
	
get_eps(Path,EPlist,PropsList) ->
	_Space = registry:get_resource_by_path(Path),
	{_,EPs,NeededResources} = lists:foldl(fun(E,{Space,FoundEPs,Resources}) -> 
					case trie:find(E,Space) of
						error -> {Space,FoundEPs,Resources};
						{ok, Value} -> {Space, [{E,Value}] ++ FoundEPs, [Value] ++ Resources}
					end
				end,
				{_Space,[],[]},
				EPlist),
	_Resources = lists:foldl(fun(E,AccIn) -> [{E,registry:get_reource(E)}] ++ AccIn end, [], lists:usort(NeededResources)),
	lists:foldl(fun({EPName,ResourceKey},{ResourceList,PropFilter,AccIn}) -> 
					case find_ep(EPName,ResourceKey,ResourceList,PropFilter) of
						nothing -> {ResourceList,PropFilter,AccIn};
						Something -> {ResourceList,PropFilter,[Something]++AccIn}
					end
				end,
				{_Resources,PropsList,[]},
				EPs).
	
find_ep(EP,ResourceKey,ResourcesList,PropFilter) ->
	case lists:keyfind(ResourceKey,1,ResourcesList) of
		false -> nothing;
		JSON ->
			case ej:get({EP},JSON) of
				undefined -> nothing;
				Value -> utils:apply_attribute_filter({EP, Value})
			end
	end.
	
remove_endpoints(Path, JSON) ->
	_Space = registry:get_resource_by_path(Path),
	_NewSpace = remove_eps(_Space,JSON),
	_SpaceResourceId = registry:get_resourceid_for_path(Path),
	updateResource(_SpaceResourceId, eppropsIndex, _NewSpace, nothing).

store_endpoints(Path,ResourceKey,Data) when is_list(Path) ->
	store_endpoints(list_to_binary(Path),ResourceKey,Data);	
store_endpoints(Path,ResourceKey,Data) when is_list(ResourceKey) ->
	store_endpoints(Path,list_to_binary(ResourceKey),Data);
store_endpoints(Path,ResourceKey,Data) ->
	%%STEP 1 - Validate
	case validate(Data) of
		{ false, Errors, _ } -> 
			lager:info("Errors found - ~p",[Errors]),
			{error, "Invalid Request"};
		{ true, _, JSON } ->
			%%Grab the space
			_Space = registry:get_resource_by_path(Path),
		
			%%STEP 2 - See if the Resource exists and remove the old 
			%%EP data before cleaning
			_LatestSpace = case registry:get_resource(ResourceKey) of
				not_found -> trie:new();
				{struct, _OldJSON} -> 
					remove_eps(_Space,_OldJSON)
			end,
			
			updateResource(ResourceKey, epprops, JSON, nothing),			
			
			%%STEP 3 - Add the EPs to the Path
			{struct, JSONAttrs} = ej:get({"endpoint-properties"},JSON),
			_NewSpace = store_eps(_LatestSpace,ResourceKey,JSONAttrs),
			_SpaceResourceId = case registry:get_resourceid_for_path(Path) of
				not_found -> 
					_NewId = << ResourceKey/bitstring, <<"-epindex">>/bitstring >>,
					registry:add_uri_mapping(Path,_NewId),
					_NewId;
				Found -> Found
			end,
			updateResource(_SpaceResourceId, eppropsIndex, _NewSpace, nothing),
			{ok, ResourceKey, JSON}
	end.
	
validate_semantics(JSON) ->
	%%TODO - Validate the map and version exist lest we be in trouble.
	%%ITERATE THROUGH LIST
	%%Validate Each Address Type is supported and build it to a list
	{struct, _Attributes} = ej:get({"endpoint-properties"}, JSON),
	_Errors = lists:foldl(fun({Name,_},AccIn) -> 
					case valid_type(Name) of
						true -> AccIn;
						false -> [Name] ++ AccIn
					end
				end,
				[],
				_Attributes),
	{ (length(_Errors) == 0), _Errors, JSON }.
	
valid_type(EPAddress) when is_binary(EPAddress) ->
	[Type,_] = binary:split(EPAddress,[<<":">>]),
	case Type of 
		<<"ipv4">> -> true;
		<<"ipv6">> -> true;
		_ -> false
	end;
valid_type(EPAddress) when is_list(EPAddress) ->
	[Type,_] = string:tokens(EPAddress,":"),
	case Type of 
		"ipv4" -> true;
		"ipv6" -> true;
		_ -> false
	end.
	
validate(JSON) ->
	case weak_validate_syntax(JSON) of
		{ok, Body} -> 
			lager:info("Map passed weak validation test",[]),
			_Res = validate_semantics(Body),
			lager:info("Will return ~p for syntax validation",[_Res]),
			_Res;
		SomethingElse -> 
			lager:info("Did not pass weak validation check",[]),
			SomethingElse
	end.	
	
store_eps(SomeValue,ResourceId,SomeList) when is_tuple(SomeValue) ->
	store_eps(trie:new(), ResourceId, SomeList);
store_eps(Space,_,[]) ->
	Space;
store_eps(Space,ResourceId,[{Name,_}|T]=List) when is_list(List) ->
	case is_binary(Name) of
		true -> store_eps( trie:store(binary_to_list(Name),ResourceId,Space), ResourceId, T);
		false -> store_eps( trie:store(Name,ResourceId,Space), ResourceId, T)
	end;
store_eps(Space,ResourceId,[H|T]=List) when is_list(List) ->
	case is_binary(H) of
		true -> store_eps( trie:store(binary_to_list(H),ResourceId,Space), ResourceId, T);
		false -> store_eps( trie:store(H,ResourceId,Space), ResourceId, T)
	end.
	
store_ep(Space,ResourceId,EPAddress) when is_tuple(Space) ->
	store_ep(trie:new(), ResourceId, EPAddress);
store_ep(Space,ResourceId,EPAddress) when is_binary(EPAddress) ->
	trie:store(binary_to_list(EPAddress),ResourceId,Space);
store_ep(Space,ResourceId,EPAddress) ->
	trie:store(EPAddress,ResourceId,Space).
	
remove_eps(SomeValue,SomeList) when is_tuple(SomeValue) ->
	trie:new();
remove_eps(Space,[]) ->
	Space;
remove_eps(Space,[{Name,_}|T]=List) when is_list(List) ->
	remove_eps( trie:erase(Name,Space), T);
remove_eps(Space,[H|T]=List) when is_list(List) ->
	remove_eps( trie:erase(H,Space), T).	
	
remove_ep(Space,EPAddress) when is_tuple(Space) ->
	trie:new();
remove_ep(Space,EPAddress) ->	
	trie:erase(EPAddress,Space).
