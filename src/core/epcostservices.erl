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
%% @doc ALTO (RFC 7285) Endpoint Cost Services functions. 
%% This module also supports Constraints.
%%
%% @end
-module(epcostservices).

-export([init/0,
		 %%get_eps/2,
		 store_eps/3,
		 load_defaults/0
		 ]).

-define(FG, finegrained).
-define(CG, coursegrained).
-define(MIXED, mixedmode).
-define(EPCSEFPATHID, epcsefpath).	
-define(EPCSDEFFILES, epcsfiles).
-define(FILTEREXT,"info").

-include("e_alto.hrl").
		 
%%
%% @doc Performs initialization tasks for this module.
%%
init() -> ok.
	
load_defaults() ->
	utils:load_defaults("Endpoint Costs", ?EPCSDEFFILES, fun epcostservices:store_eps/3).
	
%%
%% @doc Store a EP Cost Document - Only care about fine grained
%%
store_eps(Path, ResourceKey, JSON) ->
	case commonvalidate(JSON,"EPCostmap", fun epcostservices:validate_semantics/1) of
		{ok, EPCostmap, ApplicationState} ->
			%%Get the ResourceId
			_CostMode = ej:get({"meta","cost-type","cost-mode"},EPCostmap),
			_CostMetric = ej:get({"meta","cost-type","cost-metric"},EPCostmap),
			_ResourceId = << _CostMode/bitstring, _CostMetric/bitstring >>,
			updateResource(ResourceKey, epcostmap, EPCostmap, ApplicationState),
			
			%Step 2 - update IRD
			_Metric = metrics:metric_to_record(_CostMode,_CostMetric),
			_IRD0 = metrics:updateIRD(_Metric, getIRD()),		
			_ResourceEntry = resources:resource_to_record(costmap,
							_ResourceId,
							list_to_binary(application:get_env(?APPLICATIONNAME, uri_base, "http://localhost") ++ "/" ++ Path),
							[ <<"application/alto-costmap+json">> ],
							[],
							[ {<<"cost-type-names">>, [_Metric]}, { <<"cost-constraints">>, <<"true">>}],
							[]),
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
			_FilterKey = list_to_binary(_FilterPath ++ ?FILTEREXT),
			_NewValue = case e_alto_backend:get_constant( _FilterKey ) of
				not_found ->  [ {metrics:metric_to_EJSON(_Metric), ResourceKey} ];
				Value -> metrics:addToSet(_Metric, ResourceKey, Value)
			end,
			e_alto_backend:set_constant(_FilterKey, _NewValue),			
			
			{ok, ResourceKey, EPCostmap};
		Error ->
			Error
	end.		
		
removeResource(FilterPath, Metric) ->
	_FilterKey = list_to_binary(FilterPath ++ ?FILTEREXT),
	case e_alto_backend:get_constant( _FilterKey ) of
		not_found ->  ok;
		Value -> 
			_Value = metrics:removeFromSet(Metric, Value),
			case (length(_Value) > 0) of 
			 false -> ok;
			 true -> e_alto_backend:set_constant(_FilterKey, _Value)
			end
	end.
			
%%
%% @doc Gets a EP Cost Document
%%
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Endpoint Cost Map validation support.
validate_semantics(Costmap) ->
	costmap_utils:validate_Xcostmap(Costmap,{<<"endpoint-cost-map">>},fun utils:ep_validate/2,nothing).

