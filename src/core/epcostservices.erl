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
		 filter_epcs/2,
		 store_epcs/3,
		 load_defaults/0,
		 validate_semantics/1
		 ]).

-define(EPCSEFPATHID, epcsfpath).	
-define(EPCSDEFFILES, epcsfiles).
-define(FILTEREXT,"filterinfo").

-include("e_alto.hrl").
		 
%%
%% @doc Performs initialization tasks for this module.
%%
init() -> ok.
	
load_defaults() ->
	utils:load_defaults("Endpoint Costs", ?EPCSDEFFILES, fun epcostservices:store_epcs/3).

%%
%% @doc Store a EP Cost Document - Only care about fine grained
%%
store_epcs(Path, ResourceKey, JSON) when is_list(ResourceKey) ->
	store_epcs(Path, list_to_binary(ResourceKey), JSON);
store_epcs(Path, ResourceKey, JSON) ->
	case utils:commonvalidate(JSON,"EPCostmap", fun epcostservices:validate_semantics/1) of
		{ok, EPCostmap, ApplicationState} ->
			%%Get the ResourceId
			_CostMode = ej:get({"meta","cost-type","cost-mode"},EPCostmap),
			_CostMetric = ej:get({"meta","cost-type","cost-metric"},EPCostmap),
			_NextId = resources:next_id(),
			_ResourceId = << _NextId/binary, <<"_">>/binary, _CostMode/bitstring, _CostMetric/bitstring >>,
			registry:updateResource(ResourceKey, epcostmap, EPCostmap, ApplicationState),
			
			%Step 2 - update IRD
			_Metric = metrics:metric_to_record(_CostMode,_CostMetric),
			_IRD0 = metrics:updateIRD(_Metric, registry:getIRD()),		
			_ResourceEntry = resources:resource_to_record(costmap,
							_ResourceId,
							list_to_binary(application:get_env(?APPLICATIONNAME, uri_base, "http://localhost") ++ Path),
							<<"application/alto-endpointcost+json">>,
							<<"application/alto-endpointcostparams+json">>,
							[ {<<"cost-type-names">>, [_Metric]}, { <<"cost-constraints">>, true}],
							[]),
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
			_FilterKey = list_to_binary(_FilterPath ++ ?FILTEREXT),
			_NewValue = case e_alto_backend:get_constant( _FilterKey ) of
				not_found ->  [ {metrics:metric_to_EJSON(_Metric), ResourceKey} ];
				{_, Value} -> metrics:addToSet(_Metric, ResourceKey, Value)
			end,
			e_alto_backend:set_constant(_FilterKey, _NewValue),			
	
			%Step 5 - Add this costmap to the metric specific search space
			metrics:addToIndex(_Metric, epcostmap, ?FG, ResourceKey, undefined, undefined),
			
			{ok, ResourceKey, EPCostmap};
		{?ALTO_ERR, ErrCode, ErrMessage} ->
			{?ALTO_ERR, ErrCode, ErrMessage}
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
filter_epcs(Path, InputParameters) ->
	costmap_utils:filter_Xcostmap(Path, InputParameters, "endpoints", "endpoint-cost-map", fun utils:invalid_eps/1, fun utils:invalid_eps_asError/1).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Endpoint Cost Map validation support.
validate_semantics(Costmap) ->
	costmap_utils:validate_Xcostmap(Costmap,{<<"endpoint-cost-map">>},fun utils:valid_ep/2,nothing).
