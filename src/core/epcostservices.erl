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
		 store_eps/2
		 %%load_defaults/0
		 ]).

-define(FG, finegrained).
-define(CG, coursegrained).
-define(MIXED, mixedmode).
-define(EPSCFG, epsconfig).

-define(FILTEREXT,"info").

-include("e_alto.hrl").
		 
%%
%% @doc Performs initialization tasks for this module.
%%
init() -> ok.
	
%%
%% @doc Store a EP Cost Document
%%
store_eps(Path, JSON) ->
	case commonvalidate(JSON,"EPCostmap", fun epcostservices:validate_semantics/1) of
		{ok, EPCostmap, ApplicationState} ->
			%%Get the ResourceId
			_CostMode = ej:get({"meta","cost-type","cost-mode"},EPCostmap),
			_CostMetric = ej:get({"meta","cost-type","cost-metric"},EPCostmap),
			_ResourceId = << _CostMode/bitstring, _CostMetric/bitstring >>,
			updateResource(_ResourceId, epcostmap, EPCostmap, ApplicationState),
			lager:info("Map ~p has been stored. Updating IRD",[_ResourceId]),
			
			{ok, _ResourceId, EPCostmap};
		Error ->
			Error
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


