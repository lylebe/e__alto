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

%% TODO - Convert Filterspec to using the metric record list instead of
%% the EJSON representation.

-export([init/0,
		 validate_semantics/1,
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
		 
		 load_defaults/0
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
			lager:info("~p--Load CostMap -- Read complete - Starting Storage",[?MODULE]),	
			{ok, _ResourceId, X} = costmapservices:store_costmap( string:sub_string(Path,2),_File),
			lager:info("~p--Load Default Map -- Completed",[?MODULE]),
			lager:info("Resulting Costmap -> ~n~n~p~n~n~n~n~n~n",[X]),
			{Path, X};
		{error, Value} ->
			lager:info("An error occurred reading the file - ~p",[Value]),
			{Path, error}
	end.	

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
			_Metric = metrics:metric_to_record(_CostMode,_CostMetric),
			_IRD0 = metrics:updateIRD(_Metric, getIRD()),		
			_ResourceEntry = resources:resource_to_record(costmap,
							_ResourceId,
							list_to_binary(application:get_env(?APPLICATIONNAME, uri_base, "http://localhost") ++ "/" ++ Path),
							[ <<"application/alto-costmap+json">> ],
							[],
							[ {<<"cost-type-names">>, [_Metric]} ],
							[_MapId]),
			_IRD1 = resources:updateIRD(_ResourceEntry,_IRD0),				
			updateIRD( _IRD1 ),
			lager:info("IRD Updated to ~n~n~p~n~n~n",[getIRD()]),

			%Step 3 - Add URI Mapping to Registry
			_Path = registry:extract_path(ej:get({<<"resources">>,_ResourceId,<<"uri">>},_IRD1)),
			lager:info("HTTP URI is ~p for Resource ~p",[_Path,_ResourceId]),
			registry:add_uri_mapping(_Path,_ResourceId),
			
			%Step 4 - Set the FilterInfo for the URI
			_FilterPath = case lists:nth(1,Path) of
				47 -> Path;
				_ -> "/" ++ Path
			end,
			e_alto_backend:set_constant( list_to_binary(_FilterPath ++ ?FILTEREXT), [ {metrics:metric_to_EJSON(_Metric), _ResourceId} ]),
			{ok, _ResourceId, Costmap};
		Error ->
			Error
	end.

%%
%% Filter Info - This is a list of {MetricInformation, ResourceId} entries where
%% - MetricInformation is the parsed Meta inforamatio of the metric
%% - ResourceId is the internal ResourceId associated with the cost map
%%
contains_filterspec(undefined, _, _) ->
	{false, nothing};
contains_filterspec([], _, _) ->
	{false, nothing};
contains_filterspec([{ {_,MetaInfo},_ResourceId}|T], CostMetric, CostMode) ->
	lager:info("~p is the value",[MetaInfo]),
	case ((ej:get({<<"cost-metric">>},MetaInfo) == CostMetric) and
		  (ej:get({<<"cost-mode">>},MetaInfo) == CostMode)) of
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
	commonvalidate(JSON,"CostMap",fun costmapservices:validate_semantics/1).

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
			costmap_utils:validate_Xcostmap(Costmap,{<<"cost-map">>},fun lists:member/2,_PidNames)
	end.

%%%%%%%%%%%%%%%%%%%%
%% Filtering
%%%%%%%%%%%%%%%%%%%%

is_valid_filter(Filter) ->
	costmap_utils:is_valid_filter(Filter, "pids", fun utils:invalid_pidnames/1).

%%
%% @doc Retrieves Information based upon the JSON provided request.  
%% 	
filter_costmap(Path, InputParameters) ->
	costmap_utils:filter_Xcostmap(Path, InputParameters, "pids", "cost-map", fun utils:invalid_pidnames/1).
