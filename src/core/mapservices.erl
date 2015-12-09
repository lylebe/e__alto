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
		 validate/1,
		 validate_semantics/1,
		 validate_request_semantics/1,
		 pidroutes_tolist/6,
		 store_map/3,
		 load_defaults/0,
		 set_default/1,
		 is_valid_filter/1,
		 filter_map/2,
		 getPidForAddress/2,
		 load_default_map/0
		]).

-define(DEFMAPKEY, <<"defaultmap">>).
-define(DEFMAP, defaultmap).
-define(MAPSTOLOAD, maps).

-include("e_alto.hrl").

load_defaults() ->
	_Val1 = load_default_map(),
	[_Val1] ++ utils:load_defaults("Network Maps", ?MAPSTOLOAD, fun mapservices:store_map/3).

load_default_map() ->
	{ _DefMapPath, _DefMapLoc } = utils:get_param(?DEFMAP),	
	case utils:load_file( fun mapservices:store_map/3, _DefMapPath, _DefMapLoc ) of
		{Path, _ResourceId, Map} ->
			set_default(_ResourceId),
			{Path, Map};
		{Path, error} ->
			{Path, error}
	end.	

set_default(MapName) when is_list(MapName) ->
	set_default( list_to_binary(MapName) );
set_default(MapName) when is_binary(MapName) ->
	registry:updateIRD( ej:set({"meta","default-alto-network-map"}, registry:getIRD(), MapName)),
	e_alto_backend:set_constant(?DEFMAPKEY,MapName). 

store_map(Path,_,JSON) ->
	lager:info("JSON Value is ~p",[validate(JSON)]),
	case validate(JSON) of
		{ok, Map, V4ApplicationState, V6ApplicationState} ->
			%%Get the ResourceId and tag
			_ResourceId = ej:get({"meta","vtag","resource-id"},Map),
			_Tag = ej:get({"meta","vtag","tag"},Map),
			registry:updateResource(_ResourceId, _Tag, map, Map, { V4ApplicationState, V6ApplicationState }),

			%Step 2 - update IRD
			_ResourceEntry = resources:resource_to_record(networkmap,
				_ResourceId,
				list_to_binary(application:get_env(?APPLICATIONNAME, uri_base, "http://localhost") ++ Path),
				<<"application/alto-networkmap+json">>,
				undefined,
				[],
				[]),
			_IRD = resources:updateIRD(_ResourceEntry,registry:getIRD()),				
			registry:updateIRD( _IRD ),
			lager:info("IRD Updated to ~n~n~p~n~n~n",[registry:getIRD()]),

			%Step 3 - Add URI Mapping to Registry
			_Path = registry:extract_path(ej:get({<<"resources">>,_ResourceId,<<"uri">>},_IRD)),
			lager:info("HTTP URI is ~p for Resource ~p",[_Path,_ResourceId]),
			registry:add_uri_mapping(_Path,_ResourceId),

			{ok, _ResourceId, Map};
		Error ->
			Error
	end.

getPidForAddress(Address,State) when is_binary(Address) ->
	getPidForAddress(binary_to_list(Address), State);
getPidForAddress(Address,{ V4ApplicationState, V6ApplicationState }) ->
	_Pos = string:chr(Address,$:),
	Type = string:sub_string(Address,1,_Pos-1),
	Value = string:sub_string(Address,_Pos+1),
	BitStringValue = route_utils:as_bitstring(Value),
	_X = case Type of 
		"ipv4" -> trie:find_prefix_longest(BitStringValue,V4ApplicationState);
		"ipv6" -> trie:find_prefix_longest(BitStringValue,V6ApplicationState);
		_ -> {ok, undefined, undefined }
	end,
	case _X of
		{ok, _, _RetValue} -> _RetValue;
		_Other -> undefined
	end.

%%
%% @doc Retrieves the current version of the default map
%%
get_map() ->
	case e_alto_backend:get_constant(?DEFMAPKEY) of 
		{_,ResourceId} -> get_map(ResourceId);
		_ -> not_found
	end.

%%
%% @doc Retrieves the latest version of the specified map.
%%
get_map(MapIdentifier) ->
	registry:get_resource(MapIdentifier).

%%
%% @doc Get the specific version of the map
%%
get_map(MapIdentifier, Vtag) ->
	registry:get_resource(MapIdentifier,Vtag).

%%
%% @doc Retrieves Pids based upon the JSON provided request.  
%% 
get_map_by_filter(Path, InputParameters) ->
	case is_valid_filter(InputParameters) of
		{true, ParsedBody} ->
			case registry:get_resourceid_for_path(Path) of
				not_found -> not_found;
				{_, _ResourceId} -> filter_map(get_map(_ResourceId),ParsedBody)
			end;
		{false, SomeError} -> {error, SomeError};  %%TECH DEBT - < 1$ Would love to collapse this line and the next one some day...
		SomethingElse -> {error, SomethingElse}
	end.	
	
filter_map(not_found, _) ->
	not_found;
filter_map(NetworkMap, InputParameters) ->
	Z = ej:get({<<"pids">>},InputParameters),
	Y = ej:get({<<"address-types">>},InputParameters),
	lager:info("Input Parameters are ~p and ~p",[Z,Y]),
	A = { struct, [{<<"meta">>, {struct, []}},	
				   {<<"network-map">>, filter_pids( ej:get({<<"pids">>},InputParameters), 
													 NetworkMap, 
													 ej:get({<<"address-types">>},InputParameters) 
													 ) }]},	
	ej:set({<<"meta">>,<<"vtag">>}, A, ej:get({<<"meta">>,<<"vtag">>},NetworkMap)).

filter_pids(undefined, NetworkMap, []) ->
	NetworkMap;
filter_pids([], NetworkMap, []) ->
	NetworkMap;
filter_pids([], NetworkMap, undefined) ->	
	NetworkMap;
filter_pids([], NetworkMap, AddressTypeFilter) ->
	%No PIDs are to be filtered
	{struct,_Pids} = ej:get({<<"network-map">>},NetworkMap),
	{struct, utils:apply_attribute_filter_to_list(_Pids, AddressTypeFilter,true)};
filter_pids(undefined, NetworkMap, AddressTypeFilter) ->
	%If the attribute is not present we merely treat it as an empty array
	%(see case above).
	filter_pids([], NetworkMap, AddressTypeFilter);
filter_pids(PIDFilter, NetworkMap, AddressTypeFilter) ->
	{struct, filter_pids(PIDFilter, NetworkMap, AddressTypeFilter, [])}.
	
filter_pids([], _, _, AccIn) ->
	AccIn;
filter_pids([H|T], NetworkMap, AddressTypeFilter, AccIn) ->
	case ej:get({<<"network-map">>,H},NetworkMap) of
		undefined -> %When a PID is missing we keep processing
			filter_pids(T, NetworkMap, AddressTypeFilter, AccIn);
		Value ->
			case utils:apply_attribute_filter(AddressTypeFilter, [], Value, true) of
				[] ->
					filter_pids(T, NetworkMap, AddressTypeFilter, AccIn);
				NewValue ->
					filter_pids(T, NetworkMap, AddressTypeFilter, [{H,NewValue}] ++ AccIn)
			end
	end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Network Map Validation Support Functions	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_routes(undefined,_,AccIn) ->
	AccIn;
add_routes([],_,AccIn) ->
	AccIn;
add_routes([H|T],PidName,AccIn) ->
	add_routes(T,PidName,[{H,PidName}]++AccIn).

pidroutes_tolist([], RoutesV4, RoutesV6, TreeInV4, TreeInV6, Duplicates) ->
	{ RoutesV4, RoutesV6, TreeInV4, TreeInV6, Duplicates };
pidroutes_tolist([{PidName,Attributes} | Tail], RoutesV4, RoutesV6, TreeInV4, TreeInV6, Duplicates) ->
	_v4Routes = add_routes(ej:get({<<"ipv4">>}, Attributes),PidName,[]),
	{ NewV4Tree, NewDuplicates } = route_utils:insert_route_interval(_v4Routes, TreeInV4, Duplicates),
	_v6Routes = add_routes(ej:get({<<"ipv6">>}, Attributes),PidName,[]),
	{ NewV6Tree, NewDuplicates2 } = route_utils:insert_route_interval(_v6Routes, TreeInV6, NewDuplicates), 
	pidroutes_tolist(Tail, _v4Routes ++ RoutesV4, _v6Routes ++ RoutesV6, NewV4Tree, NewV6Tree, NewDuplicates2).	

validate(JSON) ->
	utils:commonvalidate(JSON,"Map",fun mapservices:validate_semantics/1). 

%Validates and Builds Map Data	
validate_semantics(NetworkMap) ->
	%%Ensure the tag is set
	_Errors =  utils:check_fields([ { {"meta","vtag","resource-id"},NetworkMap,<<"#/meta/vtag/resource-id attribute was not present in Map">> },
				{ {"meta","vtag","tag"},NetworkMap,<<"#/meta/vtag/tag attribute was not present in Map">> } ] ),	
	lager:info("_Errors are ~p",[_Errors]),
	case  utils:field_present({<<"network-map">>},NetworkMap,<<"#/network-map attribute was not present in Map">>) of
		true -> validate_mapbody(NetworkMap,_Errors);
		Error -> [Error] ++ _Errors
	end.

add_ifnotempty(Errors,ErrFun,Verbose,Type,SubType,Acc) ->
	case length(Errors) of
		0 -> Acc;
		_ -> [{Type,SubType,ErrFun(Errors,Verbose,"")}] ++ Acc
	end.
	
validate_mapbody(NetworkMap, AccErrors) ->
	% We are looking for anwhere there is an overlap of routes that is not a containment
	{struct, _Pids } = ej:get({<<"network-map">>}, NetworkMap),
	
	%Build the Route Structures for the condition checks
	% Condition 1 - same route in two locations - how to resolve - Duplicate detection for insertion of the same key in a gb_tree.
	{ PidRoutesListV4, PidRoutesListV6, PidRoutesV4Tree, PidRoutesV6Tree, Duplicates } = pidroutes_tolist(_Pids, [], [], gb_trees:empty(), gb_trees:empty(), []),
	_AccErrors1 = add_ifnotempty(Duplicates, fun route_utils:errors_to_string/3, false, ?ALTO_ERR, ?E_INVALID_FIELD_VALUE, AccErrors),

	% Condition 2 - partially overlapping routes (not proper containment relationship which is allowed) - Interval Tree is used
	{_,IntervalV4Tree} = route_utils:set_max_value(PidRoutesV4Tree, PidRoutesV4Tree),
	Overlaps = route_utils:detect_overlaps(IntervalV4Tree), 
	_AccErrors2 = add_ifnotempty(Overlaps, fun route_utils:errors_to_string/3, false, ?ALTO_ERR, ?E_INVALID_FIELD_VALUE, _AccErrors1),

	{_,IntervalV6Tree} = route_utils:set_max_value(PidRoutesV6Tree, PidRoutesV6Tree),
	OverlapsV6 = route_utils:detect_overlaps(IntervalV6Tree), 
	_AccErrors3 = add_ifnotempty(OverlapsV6, fun route_utils:errors_to_string/3, false, ?ALTO_ERR, ?E_INVALID_FIELD_VALUE, _AccErrors2),

    % Last Task is to build a trie that can perform longest common subsquence of the route data	
    case (length(_AccErrors3) > 0) of 
		false -> % build the prefix tree...
		    { ok, NetworkMap, 
				route_utils:build_bitstring_trie( PidRoutesListV4, trie:new() ),
				route_utils:build_bitstring_trie( PidRoutesListV6, trie:new() ) };
		true -> 
			_AccErrors3
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Validate a Map Filtering POST's body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_valid_filter(JSON) ->
	utils:commonvalidate(JSON,"Map",fun mapservices:validate_request_semantics/1). 

validate_request_semantics(Request) ->
	case utils:field_present({"pids"},Request,<<"#/pids attribute was not present in Map Filter Request">>) of 
		true -> {true, Request};
		Error -> {false, Error}
	end.
