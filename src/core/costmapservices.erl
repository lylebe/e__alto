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
%% @doc ALTO (RFC 7285) Cost Map Services functions (including Filtered 
%% Cost Maps).  This module also supports Constraints.
%%
%% @end
-module(costmapservices).

-export([init/0,
		 get_costmap/1, 
		 get_costmap/2, 
		 
		 get_costmap_by_path/3,
		 get_costmap_by_path/4,

		 is_registered/3,

		 get_id_for_path/3,
		 
		 filter_costmap/2,

		 register_mapping/3,
		 register_mapping/4,
		 deregister_mapping/3,
		 
		 store_costmap/2,
		 load_costmap/1,
		 
		 load_defaults/0,
		 is_valid_filter/1
		 
		]).

-define(DEFCMAPS,costmaps).
-define(FILTEREXT, "filterinfo").

-include("e_alto.hrl").

load_defaults() ->
	lager:info("~p--Loading Default Cost Maps--Starting Load",[?MODULE]),
	lists:foldl(fun(E,AccIn) -> [load_costmap(E)] ++ AccIn end, [], get_param(?DEFCMAPS)).
	
load_costmap({Path,FileLoc}) ->
	lager:info("~p--Loading CostMap -- ~p -- Beginning File Read at location ~p",[?MODULE,Path,FileLoc]),	
	case file:read_file(FileLoc) of
		{ok, _File} ->
			lager:info("~p--Load Default Map -- Read complete - Starting Storage",[?MODULE]),	
			{ok, _ResourceId, X} = costmapservices:store_costmap( string:sub_string(Path,2),_File),
			lager:info("~p--Load Default Map -- Completed",[?MODULE]),
			lager:info("Resulting Costmap -> ~n~n~p~n~n~n~n~n~n",[X]),
			{Path, X};
		{error, Value} ->
			lager:info("An error occurred reading the file - ~p",[Value]),
			{Path, error}
	end.	

generate_irdinfo(CostName,CostMode,CostMetric,MapId) when is_binary(CostName) ->
	_CostName1 = <<CostName/bitstring,<<"-cost-map">>/bitstring >>,
	{ generate_metainfo(CostName,CostMode,CostMetric,""), 
	  generate_resourceinfo(_CostName1,MapId,CostMode,CostMetric,generate_path(<<"">>, CostMode, CostMetric)) }.

generate_metainfo(CostName,CostMode,CostMetric,Description) ->
	_MetricName = << CostMode/bitstring, <<"-">>/bitstring, CostMetric/bitstring >>, 
	_Part1 = "\"" ++ binary_to_list(_MetricName) ++ "\" : {
		\"cost-mode\":\"" ++ binary_to_list(CostMode) ++ "\",
		\"cost-metric\":\"" ++ binary_to_list(CostMetric) ++ "\"",
	_Part2 = case is_list(Description) of 
			true -> ",\n\t\t\"" ++ Description ++ "\"";
			false -> ""
	end,
	_MetaPart = _Part1 ++ _Part2 ++ "}",
	{CostName, _MetaPart}.

generate_resourceinfo(CostName,MapId,CostMode,CostMetric,Path) when is_binary(Path) ->
	generate_resourceinfo(CostName,MapId,CostMode,CostMetric,binary_to_list(Path));
generate_resourceinfo(CostName,MapId,CostMode,CostMetric,Path) ->	
	_MetricName = << CostMode/bitstring, <<"-">>/bitstring, CostMetric/bitstring >>,
	_Val = "\"" ++ binary_to_list(CostName) ++ "\" : {
		\"uri\" : \"" ++ application:get_env(?APPLICATIONNAME, uri_base, "http://localhost") ++ "/" ++ Path ++ "\",
		\"media-type\" : \"application/alto-costmap+json\",
		\"capabilities\" : {
			\"cost-type-names\" : [ \"" ++ binary_to_list(_MetricName) ++ "\" ],
			\"cost-constraints\" : true 
		},
		\"uses\": [ \"" ++ binary_to_list(MapId) ++ "\" ] } ",
	{CostName, _Val}.	

%%
%% @doc Stores the Cost Map information.  
%%
%% Name is the resource name for the CostMap 
%%
%% IRD represents the IRD associated with the CostMap
%%
%% CostMap - the actual CostMap data
%% @end
%% 
store_costmap(Path,JSON) ->
	case validate(JSON) of
		{ok, Costmap, ApplicationState} ->
			%%Get the ResourceId and tag
			_MapId = ej:get({"meta","dependent-vtags",1,"resource-id"},Costmap),
		
			_MapTag = ej:get({"meta","dependent-vtags",1,"tag"},Costmap),
			
			_CostMode = ej:get({"meta","cost-type","cost-mode"},Costmap),
			_CostMetric = ej:get({"meta","cost-type","cost-metric"},Costmap),
			_ResourceId = << _CostMode/bitstring, _CostMetric/bitstring >>,
			updateResource(_ResourceId, _MapTag, costmap, Costmap, ApplicationState),
			lager:info("Map ~p has been stored. Updating IRD",[_ResourceId]),
			
			%Step 2 - update IRD
			Separator = <<"-">>,
			_CostName = << _CostMode/bitstring, Separator/bitstring, _CostMetric/bitstring >>,
			_MetaInfo = generate_resourceinfo(_CostName,_MapId,_CostMode,_CostMetric,Path),
			_IRD1 = ej:set({"resources",_ResourceId}, getIRD(), 
							ej:get({element(1,_MetaInfo)},
								 mochijson2:decode("{" ++ element(2,_MetaInfo) ++ "}" ))),
							
			_ResourceInfo = generate_metainfo(_CostName,_CostMode,_CostMetric,undefined),
			_MetricMetaInfo = mochijson2:decode("{" ++ element(2,_ResourceInfo) ++ "}"),				
			_IRD2 = ej:set({"meta","cost-types",_CostName}, _IRD1, 
						ej:get({element(1,_ResourceInfo)}, _MetricMetaInfo )),
			updateIRD( _IRD2 ),
			lager:info("IRD Updated to ~n~n~p~n~n~n",[getIRD()]),

			%Step 3 - Add URI Mapping to Registry
			_Path = registry:extract_path(ej:get({<<"resources">>,_ResourceId,<<"uri">>},_IRD2)),
			lager:info("HTTP URI is ~p for Resource ~p",[_Path,_ResourceId]),
			registry:add_uri_mapping(_Path,_ResourceId),
			
			%Step 4 - Set the FilterInfo for the URI
			_FilterPath = case lists:nth(1,Path) of
				47 -> Path;
				_ -> "/" ++ Path
			end,
			e_alto_backend:set_constant( list_to_binary(_FilterPath ++ ?FILTEREXT), [ {_MetricMetaInfo, _ResourceId} ]),
			
			{ok, _ResourceId, Costmap};
		Error ->
			Error
	end.

%%
%% Filter Info - This is a list of {MetricInformation, ResourceId} entries where
%% - MetricInformation is the parsed Meta inforamatio of the metric
%% - ResourceId is the internal ResourceId associated with the cost map
%%
contains_filterspec(undefined, CostMetric, CostMode) ->
	{false, nothing};
contains_filterspec([], CostMetric, CostMode) ->
	{false, nothing};
contains_filterspec([{ {struct,[{Name,_}]}=_MetaInfo,_ResourceId}|T], CostMetric, CostMode) ->
	case ((ej:get({Name,<<"cost-metric">>},_MetaInfo) == CostMetric) and
		  (ej:get({Name,<<"cost-mode">>},_MetaInfo) == CostMode)) of
		true -> {true, _ResourceId };
		false -> contains_filterspec(T, CostMetric, CostMode)
	end.

get_costmap(Path, CostMetric, CostMode) ->
	case get_filterinfo(Path) of
		not_found ->
			lager:info("Error - No filter information found for Path ~p",[Path]),
			not_found;
		_FilterInfo ->
			case contains_filterspec(_FilterInfo, CostMetric, CostMode) of
				{false, nothing} ->
					lager:info("No filter specification found for ~p that matches CostMode/CostMetric in request",[Path]),
					{ not_found, "No matching filter specification found"};
				{true, _ResourceId} ->
					registry:get_resource(_ResourceId)
			end
	end.

get_filterinfo(Path) when is_binary(Path) ->
	{_,Spec}=e_alto_backend:get_constant(<< Path/bitstring, << ?FILTEREXT >>/ bitstring >>),
	Spec;
get_filterinfo(Path) when is_list(Path) ->
	{_,Spec}=e_alto_backend:get_constant(list_to_binary(Path ++ ?FILTEREXT)),
	Spec.

%%
%% Internally, paths of URIs are mapped to costmaps.
%%  
%% When a CostMap is added to the system a path on the server
%% is auto generated of the form 
%%
%%  	{Path of URI used to POST}/{CostMode}/{CostType}
%%
%% If a URI was supplied when the CostMap is registered it is stored
%% in the backend using the URI otherwise the generated path is used.
%%
%% When the resource is added 
%% a. the base path is added to a CostMap URI Table (Path => ResourceID)
%%	(if it was supplied)
%% b. the generated path is added to the CostMap URI Table (always)
%%
%% This module supports CostMap Versions
%%
%% The CostMap Filter Service API allows adding cost maps IF
%% it uses the same network-map in its dependent vtags.
%% Add it generates an alias mapping.  
%%
%% If a costmap with a different base path already exists but 
%% another should replace it, then a force map option is available.
%% 
 
%%
%% @doc Performs initialization tasks for this module.
%%
init() ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic Read / Query Operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Retrieves the latest version of the specified Cost Map.
%%
get_costmap(CostmapIdentifier) ->
	registry:get_resource(CostmapIdentifier).

%%
%% @doc Get the specific version of the map
%%
get_costmap(MapIdentifier, Vtag) ->
	registry:get_resource(MapIdentifier,Vtag).
	
%%
%% @doc Gets a CostMap by retrieving the generated URI Path
%%
get_costmap_by_path(BasePath,CostMode, CostMetric) ->
	registry:get_resource_by_path(generate_path(BasePath, CostMode, CostMetric)).

%%
%% @doc Gets a CostMap by retrieving the generated URI Path and Tag
%%
get_costmap_by_path(BasePath,CostMode, CostMetric, Tag) ->
	registry:get_resource_by_path(generate_path(BasePath, CostMode, CostMetric),Tag).
	
%%
%% @doc Determines if BasePath/CostMode/CostMetric is registered as a path
%%
is_registered(BasePath, CostMode, CostMetric) ->
	registry:is_registered(generate_path(BasePath, CostMode, CostMetric)).

%%
%% @doc Gets the Resource ID for the BasePath/CostMode/CostMetric path
%%
get_id_for_path(BasePath, CostMode, CostMetric) ->
	registry:get_resourceid_for_path(generate_path(BasePath, CostMode, CostMetric)).
	
%%
%% INTERNAL FUNCTION
%% Generates a URI Path based upon the CostMode and CostMetric.
%%
generate_path(CostMode, CostMetric) when is_binary(CostMode) -> 
	generate_path(binary_to_list(CostMode), CostMetric);
generate_path(CostMode, CostMetric) when is_binary(CostMetric) ->
	generate_path(CostMode, binary_to_list(CostMetric));
generate_path(CostMode, CostMetric) when is_list(CostMode) andalso is_list(CostMetric) ->
	CostMode ++ "/" ++ CostMetric.
	
%%
%% INTERNAL FUNCTION
%% Generates a Path given the base path and metric information
%%
generate_path(BasePath, CostMode, CostMetric) ->
		BasePath ++ "/" ++ generate_path(CostMode, CostMetric).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Costmap Specific Registration Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Adds Mappings of the Base Path and the auto-generated
%% path to the Resrouce Id.
%% 
register_mapping(ResourceId, BasePath, CostMode, CostMetric) ->
	registry:add_uri_mapping(ResourceId, ResourceId),
	registry:add_uri_mapping(generate_path(BasePath, CostMode, CostMetric),ResourceId),
	ResourceId. 

register_mapping(BasePath, CostMode, CostMetric) ->
	_GeneratedPath = generate_path(BasePath, CostMode, CostMetric),
	registry:add_uri_mapping(_GeneratedPath, _GeneratedPath),
	_GeneratedPath.

deregister_mapping(BasePath, CostMode, CostMetric) ->
	registry:deregister_mapping(generate_path(BasePath, CostMode, CostMetric)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

get_pids_fromMap(NetworkMap) ->
	%Get the PID Names - The Cost Map PIDS MUST come from the map
	{struct, _Pids } = ej:get({<<"network-map">>}, NetworkMap),
	lists:foldl(fun({Name,_},AccIn) -> [Name] ++ AccIn end, [], _Pids).

%%% Cost Map validation support.
validate_semantics(Costmap) ->
	%% STEP 1 - Get Reference Map
	_MapId = ej:get({"meta","dependent-vtags",1,"resource-id"},Costmap),
	_Tag = ej:get({"meta","dependent-vtags",1,"tag"},Costmap),
	case e_alto_backend:get_item(_MapId,_Tag) of 
		0 ->
			{error, 500, "500 - The dependent map and vtag could not be found on this server"};
		SearchResult ->
			{_, _, _, {_, _NetworkMap, _}, _ } = SearchResult,
			_PidNames = get_pids_fromMap(_NetworkMap),

			%Get the Cost-Mode
			_CostMode = case ej:get({<<"meta">>,<<"cost-type">>,<<"cost-mode">>},Costmap) of
				<<"numerical">> -> numerical;
				<<"ordinal">> -> ordinal;
				_ -> unknown
			end,
			case _CostMode of
				unknown ->
					_Mode = ej:get({<<"meta">>,<<"cost-type">>,<<"cost-mode">>},Costmap),
					lager:info("422-5 An unknown Cost Mode of type ~p was referenced in the document", [_Mode]),
					{error, 422, "422-5 An unknown Cost Mode was referenced in the document"};
				_ ->
					% Check to ensure the PIDs are in the network map AND the value type is consistent
					Errors = validate_costmap_rows(ej:get({<<"cost-map">>},Costmap),_CostMode,_PidNames,[]),
					case length(Errors) of
						0 ->
							{ok, Costmap, nostate};
						_ ->
							lager:info("Semantic Errors found - ~p", Errors),
							{error, 422, "422-6 Semantic Errors are present in the document"}
					end					
			end
	end.

%%
%% Validates individual Cost Map Rows
%%
validate_costmap_rows({struct,L},CostMode,NetworkPids,ErrorList) ->
	validate_costmap_rows(L,CostMode,NetworkPids,ErrorList);
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
validate_cost_values({struct,L},SrcPid,CostMode,NetworkPids,ErrorList) ->
	validate_cost_values(L,SrcPid,CostMode,NetworkPids,ErrorList);
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
    case metrics:validate_cost_metric(Attribute,SrcPid,CostMode) of
			[] -> %% no error found
				validate_cost_values(T,SrcPid,CostMode,NetworkPids,NewErrorList);
			MetricErrorList -> 
				validate_cost_values(T,SrcPid,CostMode,NetworkPids, MetricErrorList ++ NewErrorList)
	end.

%%%%%%%%%%%%%%%%%%%%
%% Filtering
%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Determines if the filter requested is valid.
%%
is_valid_filter(Filter) ->
	case weak_validate_syntax(Filter) of
		{ok, Body} -> 
			%lager:info("Value is ~p and test result is ~p~n",[ej:get({"pids"},Body), (ej:get({"pids"},Body) =/= undefined)]),
			case (ej:get({"cost-type"},Body) =/= undefined) of
				true -> 
					lager:info("~p--Is Valid Filter-Syntax validation passed",[?MODULE]),
					case valid_constraints(ej:get({"constraints"},Body),ej:get({"cost-type","cost-mode"},Body),[]) of
						{ true, empty_list } -> {true, Body, []};
						{ true, Constraints } -> {true, Body, Constraints};
						{ false, Something } -> {false, Something}
					end;
				false -> 
					lager:info("~p--Is Invalid Filter- Error - cost-type attribute was not present",[?MODULE]),
					{false, invalid_request}
			end;
		SomethingElse -> 
			lager:info("Filter did not pass weak validation check",[]),
			{false, SomethingElse}
	end.	

%%
%% @doc Validates a list of Constraints
%%
valid_constraints(undefined,_,_) ->
	{ true, empty_list};
valid_constraints(Conditions,Units,AccIn) when is_binary(Units) ->
	valid_constraints(Conditions,list_to_atom(binary_to_list(Units)),AccIn);
valid_constraints(Conditions,Units,AccIn) when is_list(Units) ->
	valid_constraints(Conditions,list_to_atom(Units),AccIn);
valid_constraints([], _, AccIn) ->
	{true, AccIn};
valid_constraints([H|T], Units, AccIn) ->
	case valid_constraint(H,Units) of
		{false, SomeValue} -> 
			lager:info("Invalid Constration found with error ~p", [atom_to_list(SomeValue)]),
			{false, SomeValue};
		{true, Constraint} ->
			valid_constraints(T,Units,[Constraint]++AccIn)
	end.

%%
%% @doc Validates an individual Constraint.
%%
valid_constraint(Condition,Units) when is_binary(Condition) ->
	valid_constraint(binary_to_list(Condition), Units);
valid_constraint(Condition,Units) when is_atom(Units) ->
	[_Operator, _Value] = string:tokens(Condition, " "),
	{_ValType,_NumValue} = to_unit(_Value),
	case (lists:member(_Operator, ["gt","lt","ge","le","eq"]) andalso (_ValType =/= undefined)) of
		false -> {false, invalid_format};
		true -> 
			case ((Units == numerical) and (_ValType == floattype)) or ((Units == ordinal) and (_ValType == inttype)) of
				true -> {true, {list_to_atom(_Operator), _NumValue}};
				false -> {false, value_type_mismatch}
			end
	end. 

%%
%% @doc Coverts the string value to the appropriate units value.  This
%% funciton only supports numeric and orginal units at this time.
%%
to_unit(L) when is_list(L) ->
	Float = (catch erlang:list_to_float(L)),
	case is_number(Float) of
		true ->  {floattype, Float};
		false ->
			Int = (catch erlang:list_to_integer(L)),
			case is_number(Int) of
				true -> {inttype, Int};
				false -> {undefined, undefined}
			end
	end.

%%
%% @doc Retrieves Information based upon the JSON provided request.  
%% 	
filter_costmap(Path, InputParameters) ->
	case is_valid_filter(InputParameters) of 
		{true, Body, Constraints} ->
			CostMetric = ej:get({"cost-type","cost-metric"},Body),
			CostMode = ej:get({"cost-type","cost-mode"},Body),
			case get_filterinfo(Path) of
				{false, SomeIssue} ->
					{error, SomeIssue};
				not_found ->
					lager:info("Error - No filter information found for Path ~p",[Path]),
					not_found;
				_FilterInfo ->
					case contains_filterspec(_FilterInfo, CostMetric, CostMode) of
						{false, nothing} ->
							{not_found, "The Costmap could not be located for the Cost Metric and Mode requested"};
						{true, not_found} ->
							{not_found, "Although the Filter request is valid the Costmap could not be located"};
						{true, _CostMapId} ->
							_CostMap = registry:get_resource(_CostMapId),
							{ struct, [{<<"meta">>, {struct,[ {<<"dependent-vtags">>, ej:get({"meta","dependent-vtags"},_CostMap)},
															  {<<"cost-type">>, ej:get({"meta","cost-type"},_CostMap)} ] } },	
										   {<<"cost-map">>, filter_sources( ej:get({<<"pids">>,<<"srcs">>},Body), 
																			ej:get({<<"pids">>,<<"dsts">>},Body), 
																			Constraints,
																			 _CostMap,
																			[]) }]}
					end
			end;
		{false, ErrMessage} ->
			{error, ErrMessage}
	end. 

filter_sources(undefined,Dsts,Constraints,CostMap,[]) ->
	{struct, Pids} = ej:get({"cost-map"},CostMap),
	{_,_,_Result} = lists:foldl(fun(E,{_Dsts,_Constraints,List}) -> {_Dsts, _Constraints, filter_dest(undefined,E,_Dsts,_Constraints,List)} end, {Dsts,Constraints,[]}, Pids),
	_Result;
filter_sources([],_,_,_,AccIn) ->
	AccIn;
filter_sources([H|T],Dsts,Constraints,CostMap,AccIn) ->
	case ej:get({"cost-map",H},CostMap) of
		undefined ->
			filter_sources(T,Dsts,Constraints,CostMap,AccIn);
		{struct, Attrs} ->
			filter_sources(T,Dsts,CostMap,Constraints, filter_dest(H, {struct,Attrs}, Dsts, Constraints, AccIn))
	end.
	
filter_dest(undefined,{Name,{struct,Attrs}},DstsFilter,Constraints,AccIn) ->
	filter_dest(Name,{struct,Attrs},DstsFilter,Constraints,AccIn);
filter_dest(Name,{struct,Attrs},DstsFilter,Constraints,AccIn) ->
	case filter_destinations(Attrs,DstsFilter,Constraints,[]) of 
		[] -> AccIn;
		AttList -> [{Name, {struct, AttList}}] ++ AccIn
	end.	
	
filter_destinations([{_,Val}=H|T],undefined,Constraints,AccIn) ->
	case meets_criteria(Constraints,Val) of
		false -> filter_destinations(T,undefined,Constraints,AccIn);
		true -> filter_destinations(T,undefined,Constraints,[H]++AccIn)
	end;
filter_destinations(Dsts,[],Constraints,_) ->
	filter_destinations(Dsts,undefined,Constraints,[]);
filter_destinations([],_,_,AccIn) ->
	AccIn;
filter_destinations([{Dest,Val}|T],DestsFilter,Constraints,AccIn) ->
	case (lists:member(Dest,DestsFilter) andalso (meets_criteria(Constraints,Val))) of
		false -> filter_destinations(T,DestsFilter,Constraints,AccIn);
		true -> filter_destinations(T,DestsFilter,Constraints,[{Dest,Val}]++AccIn)
	end.
	
meets_criteria([], Value) ->
	true;
meets_criteria([{Operator,Discriminator}|T], Value) ->
	_TestResult = 	case Operator of
		eq -> Value == Discriminator;
		le -> Value =< Discriminator;
		ge -> Value >= Discriminator;
		ne -> Value =/= Discriminator;
		lt -> Value < Discriminator;
		gt -> Value > Discriminator
	end,
	case _TestResult of
		false -> false;
		true -> meets_criteria(T,Value)
	end.
