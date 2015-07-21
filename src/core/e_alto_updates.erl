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
%% @doc Extensions of the ej module (http://github.com/set/ej) to provide
%% support for JSON Merge Patch (RFC 7396) and JSON Patch (RFC 6902).
%%
%% @end
-module(e_alto_updates).

-define(APPNAME, e_alto).
-define(ALTOSCHEMAKEY, alto_schema).

-export([init/0, patch_resource/5]).

patch_resource(ApplicationType, PatchType, ResourceId, Vtag, Body) ->
	try
		%Check to see if the Body (Patch) is JSON compliant
		_ParsedBody = mochijson2:decode(Body),
		_Result = e_alto_backend:get_latest_version(ResourceId),
		case length(_Result) of
			0 ->
				{error, 404, "No Resource Found for ResourceId"};
			_ ->
				[ { _, _, _, Target, _ } | _ ] = _Result,
				_NewBody = case PatchType of
					mergepatch ->
						ej_merge:mergepatch(Target, _ParsedBody);
					patch ->
						% NOTE - Could do a better job of returning errors
						ej_merge:patch(Target, _ParsedBody)
				end,
				case validate(ApplicationType, _NewBody) of
					{ok, ApplicationState} ->
						updateResource(ResourceId, ApplicationType, Body, ApplicationState),
						{ok, 200, ""};
					{error, Code, Message} -> 
						{error, Code, Message}
				end
		end
	catch
		error -> %Change this to catch the EXACT error
			{error, 422, "422-3 Request body (patch) included invalid JSON OR patch attempt failed"}
	end.

init() ->
	_SchemaFile = application:get_env(?APPNAME, schema, ""),
	lager:info("e_alto_updates:init - Schema file = ~p", [_SchemaFile]),
	{ok, _File} = file:read_file(_SchemaFile),
	SchemaStruct = mochijson2:decode( binary_to_list(_File) ),
	jesse:add_schema(?ALTOSCHEMAKEY, alto_schema),
	lager:info("Schema File Load Complete and stored under key ~p", [ ?ALTOSCHEMAKEY ]).

setResource(ApplicationType, ResourceId, Vtag, Body) ->
	case validate(ApplicationType, Body) of
		{ok, ApplicationState} ->
			updateResource(ResourceId, Vtag, Body, ApplicationState);
		{error, Code, Message} ->
			{error, Code, Message}
	end.

updateResource(ResourceId, ApplicationType, Body, ApplicationState) ->
	e_alto_backend:store(ResourceId, {ApplicationType,Body,ApplicationState}).
	%Not sure about the Application State yet.
		
updateResource(ResourceId, Vtag, ApplicationType, Body, ApplicationState) ->
	e_alto_backend:store(ResourceId, Vtag, {ApplicationType,Body,ApplicationState}).
	%Not sure about the Application State yet.	 
	
validate(ApplicationType, Body) when is_binary(Body) ->
	validate( ApplicationType, binary_to_list(Body) );
validate(ApplicationType, Body) ->
	% STEP - Validate against ALTO Schema
	try 
	  ParsedBody = mochijson2:decode(Body),
	  case jesse:validate(?ALTOSCHEMAKEY, ParsedBody) of
		{ok, JsonStruct} ->
				% STEP - Semantic Validation based on application type
				% also builds the new application data for the ALTO server
				case ApplicationType of
					map -> build_map_data(ParsedBody);
					endpoint -> check_endpoint_properties(ParsedBody);
					cost_map -> check_cost_map(ParsedBody);
					ep_cost_map -> validate_endpoint_costs(ParsedBody)
				end;
		_ ->
			{error, 422, "422-2 Operation result created JSON that could not pass ALTO Schema check"}
	  end
	catch 
		error ->
			{error, 422, "422-1 Operation result create invalid JSON"}
	end.
	
%% Network Map Validation Support	
add_routes(undefined,PidName,AccIn) ->
	AccIn;
add_routes([],PidName,AccIn) ->
	AccIn;
add_routes([H|T],PidName,AccIn) ->
	add_routes(T,PidName,[{H,PidName}]++AccIn).

pidroutes_tolist([], AccIn, AccTreeIn) ->
	{ AccIn, AccTreeIn };
pidroutes_tolist([{PidName,Attributes} | Tail], AccIn, AccTreeIn) ->
	_v4Routes = add_routes(ej:get({<<"ipv4">>}, Attributes),PidName,[]),
	NewTree1 = route_utils:insert_route_interval(_v4Routes, AccTreeIn),
	_v6Routes = add_routes(ej:get({<<"ipv6">>}, Attributes),PidName,[]),
	pidroutes_tolist(Tail, _v6Routes ++ _v4Routes, NewTree1).	
	
find_duplicates(List,CurrentPosition,Max,AccIn) when (CurrentPosition < Max) ->
	{V1,PID1} = lists:nth(CurrentPosition,List),
	{V2,PID2} = lists:nth(CurrentPosition+1,List),
	NewAccIn = case V1 =:= V2 of
		true -> {V1,PID1,PID2} ++ AccIn;
		false -> AccIn
	end,
	find_duplicates(List,CurrentPosition+1,Max,NewAccIn);
find_duplicates(_,_,_,AccIn) ->
	AccIn.

%Validates and Builds Map Data	
build_map_data(NetworkMap) ->
	% We are looking for anwhere there is an overlap of routes that is not a containment
	{struct, _Pids } = ej:get({<<"network-map">>}, NetworkMap),
	
	%Build the Route Structures for the condition checks
	{ PidRoutesList, PidRoutesTree } = pidroutes_tolist(_Pids,[], gb_trees:empty()),
	
	% Condition 1 - same route in two locations - how to resolve - Dictionary Duplicate detection
	SortedPidRoutes = lists:keysort(1,PidRoutesList),
	Duplicates = find_duplicates(SortedPidRoutes, 0, length(SortedPidRoutes), []),
	case length(Duplicates) of
		0 -> lager:info("No exact duplicates found");
		_ -> lager:info("Duplicates found - ~p", Duplicates)
	end,
	
	% Condition 2 - partially overlapping routes (not an interval) - Interval Tree is used
	% Convert RB tree to interval tree
	IntervalTree = route_utils:set_max_value(PidRoutesTree, PidRoutesTree),
	Overlaps = route_utils:detect_overlaps(IntervalTree), 
	case length(Overlaps) of
		0 -> lager:info("No CIDR overlaps found");
		_ -> lager:info("CIDR overlaps found - ~p", Overlaps)
	end,	
	
    % Last Task is to build a trie that can perform longest common subsquence of the route data	
    case ((length(Duplicates)>0) or (length(Overlaps))) of
		false -> % build the prefix tree...
		    {ok, route_utils:build_bitstring_trie( PidRoutesList, trie:new() )};
		true ->
			{error, 422, "422-4 Semantic Violation - Map did not pass semantic formats"}
    end.

%%% Common Checking Functions
get_MapResource(Document) ->
	% Pull the dependent resources info
	_MapResourceId = ej:get({<<"meta">>,<<"dependent-vtags">>,<<"resource-id">>},Document),
	_MapVtag = ej:get({<<"meta">>,<<"dependent-vtags">>,<<"tag">>},Document),
	e_alto_backend:get_item( _MapResourceId, _MapVtag ).	

get_pids_fromMap(NetworkMap) ->
	%Get the PID Names - The Cost Map PIDS MUST come from the map
	{struct, _Pids } = ej:get({<<"network-map">>}, NetworkMap),
	lists:foldl(fun({Name,_},AccIn) -> [Name] ++ AccIn end, [], _Pids).

%%% Cost Map validation support.
check_cost_map(CostMap) ->
	%Get Map's Pids
	SearchResult = get_MapResource(CostMap),
	case length(SearchResult) of 
		0 ->
			{error, 500, "500 - The dependent map and vtag could not be found on this server"};
		_ ->
			{_, _, _, {_, _NetworkMap, _}, _ } = lists:nth(1,SearchResult),
			_PidNames = get_pids_fromMap(_NetworkMap),

			%Get the Cost-Mode
			_CostMode = case ej:get({<<"meta">>,<<"cost-type">>,<<"cost-mode">>},CostMap) of
				"numerical" -> numerical;
				"ordinal" -> ordinal;
				_ -> unknown
			end,
			case _CostMode of
				unknown ->
					{error, 422, "422-5 An unknown Cost Mode was referenced in the document"};
				_ ->
					% Check to ensure the PIDs are in the network map AND the value type is consistent
					Errors = validate_costmap_rows(ej:get({<<"cost-map">>}),_CostMode,_PidNames,[]),
					case length(Errors) of
						true ->
							lager:info("Semantic Errors found - ~p", Errors),
							{error, 422, "422-6 Semantic Errors are present in the document"};
						false ->
							{ok, nostate}
					end					
			end
	end.

%%
%% Validates individual Cost Map Rows
%%
validate_costmap_rows([],_,_,ErrorList) ->
	ErrorList;
validate_costmap_rows([{SrcPid,L}|T],CostMode,NetworkPids,ErrorList) ->
	NewErrorList = case lists:member(SrcPid,NetworkPids) of
		false -> 
			validate_cost_values(L,SrcPid,CostMode,NetworkPids,[{src_pid_notfound, SrcPid}] ++ ErrorList);
		true -> 
			validate_cost_values(L,SrcPid,CostMode,NetworkPids,ErrorList)
	end,
	validate_costmap_rows(T,CostMode,NetworkPids,NewErrorList).
	
%%
%% Validates the cost values of a Cost Map's individual row entry by
%% a. Determining that the Destination PIDs exist in the Network Map
%%    referenced in the dependent-vtag
%% b. Determine that all cost values conform to the Cost Mode type
%%    specified
%%	
validate_cost_values([],_,_,_,ErrorList) ->
	ErrorList;
validate_cost_values([{DstPid,MetricValue}=Attribute|T],SrcPid,CostMode,NetworkPids,ErrorList) ->
	%Check the DstPid
	NewErrorList = case lists:member(DstPid,NetworkPids) of
		false -> 
			[{dst_pid_notfound, DstPid}] ++ ErrorList;
		true -> 
			ErrorList
	end,
    case validate_cost_metric(Attribute,SrcPid,CostMode) of
			[] -> %% no error found
				validate_cost_values(T,SrcPid,CostMode,NetworkPids,NewErrorList);
			MetricErrorList -> 
				validate_cost_values(T,SrcPid,CostMode,NetworkPids, MetricErrorList ++ NewErrorList)
	end.
	
%Validates the Endpoints
check_endpoint_properties(EndpointProperties) ->
	%Get Map's Pids
	SearchResult = get_MapResource(EndpointProperties),
	case length(SearchResult) of 
		0 ->
			{error, 500, "500 - The dependent map and vtag could not be found on this server"};
		_ ->
			{_, _, _, {_, _MapResource, _MapState}, _} = get_MapResource(EndpointProperties),
			_PidNames = get_pids_fromMap(_MapResource),
			
			%Get Endpoint Names
			{ struct, _EPList } = ej:get({<<"endpoint-properties">>}, EndpointProperties),
			_Endpoints = lists:foldl(fun({EPName,_},AccIn) -> [ ep_address_to_bitstring(EPName) ] ++ AccIn end, [], _EPList),
			
			%Validate that the endpoints are part of the map
			_OrphanedEndpoints = orphaned_endpoints(_Endpoints, _MapState, []),
			case length(_OrphanedEndpoints) of
				0 -> {ok, nothing};
				_ -> 
					lager:info("Endpoints found that were not part of the network map - ~p", _OrphanedEndpoints),
					{error, 422, "422-6 Endpoints were found that cannot be part of the dependent network map"}
			end
	end.
			
orphaned_endpoints([], _, AccIn) ->
	AccIn;
orphaned_endpoints([H|T], MapState, AccIn) ->
	case trie:find_prefix_longest( ep_address_to_bitstring(H), MapState ) of
		error -> orphaned_endpoints(T, MapState, [H] ++ AccIn);
		{ok, _, _} -> orphaned_endpoints(T, MapState, AccIn)
	end.		
	
ep_address_to_bitstring(EPName) ->
	{_,Address}=string:tokenize(EPName,":"),
	TupleAddress =  inet:parse_address(Address),
	NumericAddress = route_utils:ip_to_int(TupleAddress),
	io_lib:format("~2B.",[NumericAddress]).		

validate_endpoint_costs(EPCosts) -> 
	%Get the Cost-Mode
	_CostMode = case ej:get({<<"meta">>,<<"cost-type">>,<<"cost-mode">>},EPCosts) of
		"numerical" -> numerical;
		"ordinal" -> ordinal;
		_ -> unknown
	end,
	case _CostMode of
		unknown ->
			{error, 422, "422-5 An unknown Cost Mode was referenced in the document"};
		_ ->
			% Check to ensure the Endpoints have value types consistent with the cost-mode
			Errors = validate_ep_costmap_rows(ej:get({<<"endpoint-cost-map">>}),_CostMode,[]),
			case length(Errors) of
				true ->
					lager:info("Semantic Errors found - ~p", Errors),
					{error, 422, "422-6 Semantic Errors are present in the document"};
				false ->
					{ok, nostate}
			end					
	end.

%%
%% Validates the Rows of the Endpoint Cost Map
%%
validate_ep_costmap_rows([],_,ErrorList) ->
	ErrorList;
validate_ep_costmap_rows([{SrcEP,L}|T],CostMode,ErrorList) ->
	NewErrorList = lists:foldl(fun(E,AccIn) -> validate_cost_metric(E,SrcEP,CostMode) ++ AccIn end, ErrorList, L),
	validate_ep_costmap_rows(T,CostMode,NewErrorList).
	
%% 
%% Validates that the attributes's value conforms to the format (ordinal, numerical)
%% of the metric it represents.
%%	
validate_cost_metric({AttributeName,AttributeValue},JSONPath,CostMode) ->
	case is_valid_instance(CostMode, bin_to_num(AttributeValue)) of
		false ->
			[{invalid_metric_value, JSONPath ++ <<"/">> ++ AttributeName, AttributeValue}];
		true ->
			[]
	end.

%%
%% Safely changes a binary value to a numeric value
%%
bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

%%
%% Selects which test to perform to validate that the valus is of the 
%% specified metric type
%%
is_valid_instance(numerical, Val) -> 
	is_numerical(Val);
is_valid_instance(ordinal,Val) ->
	is_ordinal(Val).

%%
%% Validates ordinal metric values for conformance.
%%
is_ordinal(X) when is_integer(X) andalso X >= 0 ->
	true;
is_ordinal(_) ->
	false.
	
%%	
%% Validates numerical metric values for conformance.
%%	
is_numerical(X) when is_float(X) ->
	true;
is_numerical(X) when is_integer(X) ->
	true;
is_numerical(_) ->
	false.
