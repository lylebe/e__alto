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
	RetVal = e_alto_backend:get_lastest_version(MapIdentifier),
	case length(Retval) of
		{_, _, _, { _, Map, _ }, _ } -> Map;
		_ -> not_found
	end.

%%
%% @doc Get the specific version of the map
%%
get_map(MapIdentifier, Vtag) ->
	RetVal = e_alto_backend:get_item(MapIdentifier,Vtag),
	case length(Retval) of
		{_, _, _, { _, Map, _ }, _ } -> Map;
		_ -> not_found
	end.

%%
%% @doc Retrieves Pids based upon the JSON provided request.  
%% 
get_map_by_filter(MapIdentifier, InputParameters) ->
	filter_map( get_map(MapIdentifier), InputParameters ).

%%
%% @doc Retrieves Pids based upon the JSON provided request.  
%% 
get_map_by_filter(MapIdentifier, Vtag, InputParameters) ->
	filter_map( get_map(MapIdentifier, Vtag), InputParameters ).	
	
filter_map(not_found, _) ->
	not_found;
filter_map(NetworkMap, InputParameters) ->
	A = { struct, [{<<"meta">>, {struct, []}},	
				   {<<"network-map">>, filter_pids( ej:get({<<"pids">>},InputParameters), 
													 NetworkMap, 
													 ej:get({<<"address-types">>},InputParameters) 
													 ) }]},	
	ej:set({<<"meta">>,<<"vtag">>}, A, ej:get({<<"meta">>,<<"vtag">>},NetworkMap)).
	
	
filter_pids([], NetworkMap, AddressTypeFilter) ->
	%No PIDs are to be filtered
	{struct, _Pids} = ej:get({<<"network-map">>},NetworkMap),
	{struct, utils:apply_attribute_filter_to_list(_Pids, AddressTypeFilter)};
filter_pids(undefined, NewtorkMap, AddressTypeFilter) ->
	%If the attribute is not present we merely treat it as an empty array
	%(see case above).
	filter_pids([], NetworkMap, AddressTypeFilter);
filter_pids(PIDFilter, NetworkMap, AddressTypeFilter) 
	{struct, filter_pids(PIDFilter, NetworkMap, AddressTypeFilter, [])}.
	
filter_pids([], _, _, AccIn) ->
	AccIn;
filter_pids([H|T], NetworkMap, AddressTypeFilter, AccIn) ->
	case ej:get({<<"network-map">>,H},NetworkMap) of
		undefined -> %When a PID is missing we keep processing
			filter_pids(T, NetworkMap, AddressTypeFilter, AccIn);
		Value ->
			NewValue = utils:apply_attribute_filter(AddressTypeFilter, Value)},
			filter_pids(T, NetworkMap, AddressTypeFilter, [NewValue] ++ AccIn)
	end. 
