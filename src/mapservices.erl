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
%% @doc ALTO (RFC 7285) Map Services functions (including Filtered Map)
%%
%% @end
-module(mapservices).

-export([get_map/0, 
		 get_map/1, 
		 get_map/2, 
		 get_map_by_filter/2, 
		 get_map_by_filter/3
		]).

-include("e_alto.hrl").

%%
%% @doc Retrieves the current version of the default map
%%
get_map() ->
	get_map( application:get_env(?APPLICATIONNAME, defaultmap, "") ).

%%
%% @doc Retrieves the latest version of the specified map.
%%
get_map(MapIdentifier) ->
	{_, _, _, { _, Map, _ }, _ } = e_alto_backend:get_latest_version(MapIdentifier),
	Map.

%%
%% @doc Get the specific version of the map
%%
get_map(MapIdentifier, Vtag) ->
	{_, _, _, { _, Map, _ }, _ } = e_alto_backend:get_item(MapIdentifier, Vtag),
	Map.

%%
%% @doc Retrieves Pids based upon the JSON provided request.  
%% 
get_map_by_filter(MapIdentifier, InputParameters) ->
	_NetworkMap = e_alto_backend:get_latest_version(MapIdentifier),
	case length(_NetworkMap) of
		0 -> not_found;
		_ -> filter_map(lists:nth(1,_NetworkMap), InputParameters)
	end.

%%
%% @doc Retrieves Pids based upon the JSON provided request.  
%% 
get_map_by_filter(MapIdentifier, Vtag, InputParameters) ->
	_NetworkMap = e_alto_backend:get_item(MapIdentifier, Vtag),
	case length(_NetworkMap) of
		0 -> not_found;
		_ -> filter_map(lists:nth(1,_NetworkMap), InputParameters)

	end.	
	
filter_map(NetworkMap, InputParameters) ->
	A = { struct, [{<<"meta">>, {struct, []}},	
				   {<<"network-map">>, collect_pids( ej:get({<<"pids">>},InputParameters), 
													 NetworkMap, 
													 ej:get({<<"address-types">>},InputParameters), 
													 []) }]},	
	ej:set({<<"meta">>,<<"vtag">>}, A, ej:get({<<"meta">>,<<"vtag">>},NetworkMap)).
	
collect_pids(undefined, _, _, _) ->
	[];
collect_pids([], _, _, AccIn) ->
	AccIn;
collect_pids([H|T], NetworkMap, AddressTypeFilter, AccIn) ->
	case ej:get({<<"network-map">>,H},NetworkMap) of
		undefined -> collect_pids(T, NetworkMap, AddressTypeFilter, AccIn);
		Value ->
			NewValue = {struct, filter_attributes(AddressTypeFilter, Value, [])},
			collect_pids(T, NetworkMap, AddressTypeFilter, [NewValue] ++ AccIn)
	end. 

filter_attributes(undefined, {struct, JSONObject}, _) ->
	JSONObject;
filter_attributes(undefined, JSONObject, _) ->
	JSONObject;
filter_attributes([], _, AccIn) ->
	AccIn;
filter_attributes([H|T], JSONObject, AccIn) ->
	case ej:get({H},JSONObject) of
		undefined -> filter_attributes(T, JSONObject, AccIn);
		Value -> filter_attributes(T, JSONObject, ej:set({H},JSONObject,Value) )
	end.
