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
		 get_costmap/3,
		 get_costmap_by_path/3,
		 get_costmap_by_path/4,

		 filter_costmap/2,
		 store_costmap/3,
		 load_defaults/0
		]).

-define(DEFCMAPS,costmaps).
-define(FILTEREXT, "filterinfo").

-include("e_alto.hrl").

load_defaults() ->
	utils:load_defaults("Network Cost Maps", ?DEFCMAPS, fun costmapservices:store_costmap/3).	

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
store_costmap(Path,_,JSON) ->
	case validate(JSON) of
		{ok, Costmap, ApplicationState} ->
			%%Get the ResourceId and tag
			_MapId = ej:get({"meta","dependent-vtags",1,"resource-id"},Costmap),
			_MapTag = ej:get({"meta","dependent-vtags",1,"tag"},Costmap),
			
			_CostMode = ej:get({"meta","cost-type","cost-mode"},Costmap),
			_CostMetric = ej:get({"meta","cost-type","cost-metric"},Costmap),
			_ResourceId = << _CostMode/bitstring, _CostMetric/bitstring >>,
			registry:updateResource(_ResourceId, _MapTag, costmap, Costmap, ApplicationState),
			lager:info("Map ~p has been stored. Updating IRD",[_ResourceId]),
			
			%Step 2 - update IRD
			_Metric = metrics:metric_to_record(_CostMode,_CostMetric),
			_IRD0 = metrics:updateIRD(_Metric, registry:getIRD()),		
			_ResourceEntry = resources:resource_to_record(costmap,
							_ResourceId,
							list_to_binary(application:get_env(?APPLICATIONNAME, uri_base, "http://localhost") ++ Path),
							[ <<"application/alto-costmap+json">> ],
							[],
							[ {<<"cost-type-names">>, [_Metric]} ],
							[_MapId]),
			_IRD1 = resources:updateIRD(_ResourceEntry,_IRD0),				
			registry:updateIRD( _IRD1 ),
			lager:info("IRD Updated to ~n~n~p~n~n~n",[registry:getIRD()]),

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
			
			%Step 6 - Add the costmap to the metric
			metrics:addToIndex(_Metric, costmap, coarse, _ResourceId, _MapTag, nothing),
			
			{ok, _ResourceId, Costmap};
		{?ALTO_ERR, ErrCode, ErrMessage} ->
			{?ALTO_ERR, ErrCode, ErrMessage}
	end.

get_costmap(Path, CostMetric, CostMode) ->
	case costmap_utils:get_filterinfo(Path) of
		not_found ->
			lager:info("Error - No filter information found for Path ~p",[Path]),
			not_found;
		_FilterInfo ->			
			case costmap_utils:contains_filterspec(_FilterInfo, CostMetric, CostMode) of
				{false, nothing} ->
					lager:info("No filter specification found for ~p that matches CostMode/CostMetric in request",[Path]),
					{ not_found, "No matching filter specification found"};
				{true, _ResourceId} ->
					registry:get_resource(_ResourceId)
			end
	end.

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
	registry:get_resource_by_path(costmap_utils:generate_path(BasePath, CostMode, CostMetric)).

%%
%% @doc Gets a CostMap by retrieving the generated URI Path and Tag
%%
get_costmap_by_path(BasePath,CostMode, CostMetric, Tag) ->
	registry:get_resource_by_path(costmap_utils:generate_path(BasePath, CostMode, CostMetric),Tag).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate(JSON) ->
	utils:commonvalidate(JSON,"CostMap",fun costmapservices:validate_semantics/1).

get_pids_fromMap(NetworkMap) ->
	%Get the PID Names - The Cost Map PIDS MUST come from the map
	{struct, _Pids } = ej:get({<<"network-map">>}, NetworkMap),
	lists:foldl(fun({Name,_},AccIn) -> [Name] ++ AccIn end, [], _Pids).

%%% Cost Map validation support.
validate_semantics(Costmap) ->
	%% Get the referenced Map
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
%%
%% @doc Retrieves Information based upon the JSON provided request.  
%% 	
filter_costmap(Path, InputParameters) ->
	costmap_utils:filter_Xcostmap(Path, InputParameters, "pids", "cost-map", fun utils:invalid_pidnames/1, fun utils:invalid_pidnames_asError/1).
