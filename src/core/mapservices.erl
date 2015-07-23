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
		 set_map/2,
		 load_default_map/0,
		 is_schema_loaded/0,
		 ets_table_exists/1,
		 load_schema/0,
		 validate_syntax/1,
		 gen_path/0,
		 gen_resource_entry/1,
		 get_param/1,
		 getIRD/0,
		 set_default/1,
		 is_valid_filter/1,
		 filter_map/2
		]).

-define(DEFMAPKEY, <<"defaultmap">>).

-include("e_alto.hrl").

load_default_map() ->
	lager:info("~p--Load Default Map--Starting Load",[?MODULE]),
	_DefMapLoc = get_param(defaultmaploc),
	_DefMapPath = get_param(defaultmappath),
	lager:info("~p--Load Default Map--Begin File Read",[?MODULE]),
	{ok, _File} = file:read_file(_DefMapLoc),
	lager:info("~p--Load Default Map--Read complete - Starting Storage",[?MODULE]),	
	{ok, _ResourceId, X} = mapservices:set_map( string:sub_string(_DefMapPath,2),_File),
	set_default(_ResourceId),
	lager:info("~p--Load Default Map--Completed",[?MODULE]),
	{_DefMapPath, X}.

set_default(MapName) when is_list(MapName) ->
	set_default( list_to_binary(MapName) );
set_default(MapName) when is_binary(MapName) ->
	updateIRD( ej:set({"meta","default-alto-network-map"}, getIRD(), MapName)),
	e_alto_backend:set_constant(?DEFMAPKEY,MapName). 

gen_resource_entry(Path) when is_binary(Path) ->
	gen_resource_entry(binary_to_list(Path));	
gen_resource_entry(Path) when is_list(Path) ->
	_HostBase = get_param(hostbase),
	A = "{\"foo\" : {\n\t\"uri\" : \"" ++ _HostBase ++ "/" ++ Path ++ "\",\n\t\"media-type\" : \"application/alto-networkmap+json\"}}",
	B = mochijson2:decode(A),
	ej:get({<<"foo">>},B).

set_map(Path,JSON) ->
	case validate(JSON) of
		{ok, Map, ApplicationState} ->
			%%Get the ResourceId and tag
			_ResourceId = ej:get({"meta","vtag","resource-id"},Map),
			_Tag = ej:get({"meta","vtag","tag"},Map),
			X = updateResource(_ResourceId, _Tag, map, Map, ApplicationState),
			%Step 2 - update IRD
			_ResourceEntry = gen_resource_entry(Path),
			updateIRD( ej:set({"resources",_ResourceId}, getIRD(), 
							_ResourceEntry) ),
			%Step 3 - Add URI Mapping to Registry
			lager:info("Path is ~p for ~p",[ej:get({<<"uri">>},_ResourceEntry),_ResourceEntry]),
			_Path = registry:extract_path(ej:get({<<"uri">>},_ResourceEntry)),
			lager:info("HTTP URI is ~p for Resource ~p",[_Path,_ResourceId]),
			registry:add_uri_mapping(_Path,_ResourceId),
			{ok, _ResourceId, X};
		Error ->
			Error
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
get_map_by_filter(MapIdentifier, InputParameters) ->
	filter_map( get_map(MapIdentifier), InputParameters ).
	
	
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

%% TODO - Process IPv6 for duplicate detection
pidroutes_tolist([], AccIn, AccTreeIn, Duplicates) ->
	{ AccIn, AccTreeIn, Duplicates };
pidroutes_tolist([{PidName,Attributes} | Tail], AccIn, AccTreeIn, Duplicates) ->
	_v4Routes = add_routes(ej:get({<<"ipv4">>}, Attributes),PidName,[]),
	{ NewTree1, NewDuplicates } = route_utils:insert_route_interval(_v4Routes, AccTreeIn, Duplicates),
	_v6Routes = add_routes(ej:get({<<"ipv6">>}, Attributes),PidName,[]),
	pidroutes_tolist(Tail, _v6Routes ++ _v4Routes ++ AccIn, NewTree1, NewDuplicates).	

validate(JSON) ->
	case weak_validate_syntax(JSON) of
		{ok, Body} -> 
			lager:info("Map passed weak validation test",[]),
			_Res = validate_semantics(Body),
			lager:info("Will return ~p for syntax validation",[_Res]),
			_Res;
		SomethingElse -> 
			lager:info("Map did not pass weak validation check",[]),
			SomethingElse
	end.

%% TODO - Build IPv6 trie seperately from IPv4 trie
%Validates and Builds Map Data	
validate_semantics(NetworkMap) ->
	%%Ensure the tag is set
	case ej:get({"meta","vtag","resource-id"},NetworkMap) of
		undefined ->
			lager:info("No resource-id found",[]);
		_ ->
			ok
	end,
	
	case ej:get({"meta","vtag","tag"},NetworkMap) of
		undefined -> 
			lager:info("No tag found",[]);
		_->
			ok
	end,

	% We are looking for anwhere there is an overlap of routes that is not a containment
	{struct, _Pids } = ej:get({<<"network-map">>}, NetworkMap),
	
	%Build the Route Structures for the condition checks
	% Condition 1 - same route in two locations - how to resolve - Duplicate detection
	% for insertion of the same key in a gb_tree.
	{ PidRoutesList, PidRoutesTree, Duplicates } = pidroutes_tolist(_Pids,[], gb_trees:empty(),[]),
	
	case length(Duplicates) of
		0 -> lager:info("No exact duplicates found");
		_ -> lager:info("Duplicates found - ~p", Duplicates)
	end,
	
	% Condition 2 - partially overlapping routes (not an interval) - Interval Tree is used
	% Convert RB tree (gb_tree) to interval tree
	{_,IntervalTree} = route_utils:set_max_value(PidRoutesTree, PidRoutesTree),
	Overlaps = route_utils:detect_overlaps(IntervalTree), 
	case length(Overlaps) of
		0 -> lager:info("No CIDR overlaps found");
		_ -> lager:info("CIDR overlaps found - ~p", Overlaps)
	end,	
	
    % Last Task is to build a trie that can perform longest common subsquence of the route data	
    case ((length(Duplicates)>0) or (length(Overlaps)>0)) of
		false -> % build the prefix tree...
			lager:info("Building NetworkMap Trie for Routing",[]),
		    {ok, NetworkMap, route_utils:build_bitstring_trie( PidRoutesList, trie:new() )};
		true ->
			lager:info("Semantic violation - error will be returned",[]),
			{error, 422, "422-4 Semantic Violation - Map did not pass semantic formats"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Validate a Map Filtering POST's body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_valid_filter(JSON) ->
	case weak_validate_syntax(JSON) of
		{ok, Body} -> 
			%lager:info("Value is ~p and test result is ~p~n",[ej:get({"pids"},Body), (ej:get({"pids"},Body) =/= undefined)]),
			case (ej:get({"pids"},Body) =/= undefined) of
				true -> 
					lager:info("~p--Is Valid Filter-Syntax validation passed",[?MODULE]),
					{true, Body};
				false -> 
					lager:info("~p--Is Valid Filter- Error - pids attribute was not present",[?MODULE]),
					{false, undefined}
			end;
		SomethingElse -> 
			lager:info("Filter did not pass weak validation check",[]),
			{false, SomethingElse}
	end.
