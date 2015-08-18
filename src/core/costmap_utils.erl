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
%% @doc Generic utilities used for costmaps and endpoint costmaps
%%
%% @end
-module(costmap_utils).

-export([	
	validate_Xcostmap/4
	]).

-include("e_alto.hrl").

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Validates a Costmap or Endpoint Costmap
%%
validate_Xcostmap(Costmap,MapAccessor,VerficationFunction,VFArgs) -> 
	%Get the Cost-Mode
	_CostMode = case ej:get({<<"meta">>,<<"cost-type">>,<<"cost-mode">>},Costmap) of
		<<"numerical">> -> numerical;
		<<"ordinal">> -> ordinal;
		_ -> unknown
	end,
	case _CostMode of
		unknown ->
			lager:info("422-5 An unknown Cost Mode of type ~p was referenced in the document", [_CostMode]),
			{error, 422, "422-5 An unknown Cost Mode was referenced in the document"};
		_ ->
			% Check to ensure the PIDs are in the network map AND the value type is consistent
			Errors = validate_Xcostmap_rows(ej:get(MapAccessor,Costmap),_CostMode,VerficationFunction,VFArgs,[]),
			case length(Errors) of
				0 ->
					{ok, Costmap, nostate};
				_ ->
					lager:info("Semantic Errors found - ~p", Errors),
					{error, 422, "422-6 Semantic Errors are present in the document"}
			end					
	end.
	
%%
%% Validates individual row validation for an Endpoint Cost Map or Cost
%% map.
%%
validate_Xcostmap_rows({struct,L},CostMode,VF,VFArgs,ErrorList) ->
	validate_Xcostmap_rows(L,CostMode,VF,VFArgs,ErrorList);
validate_Xcostmap_rows([],_,_,_,ErrorList) ->
	ErrorList;
validate_Xcostmap_rows([{SrcId,L}|T],CostMode,VF,VFArgs,ErrorList) ->
	NewErrorList = case VF(SrcId,VFArgs) of
		false -> 
			validate_Xcost_values(L,SrcId,CostMode,VF,VFArgs,[{src_id_notfound, SrcId}] ++ ErrorList);
		true -> 
			validate_Xcost_values(L,SrcId,CostMode,VF,VFArgs,ErrorList)
	end,
	validate_Xcostmap_rows(T,CostMode,VF,VFArgs,NewErrorList).

%%
%% Validates the cost values of a Endpoint Cost Map or Cost Map's 
%% individual row entry by
%% For Costmaps
%% a. Determining that the Source and Destination PIDs exist in the 
%% Network Map referenced in the dependent-vtag
%% b. Determine that all cost values conform to the Cost Mode type
%%    specified
%% or for Endpoint Costmaps
%% a. Determining that the Source and Destination Endpoints are valid
%% b. Determine that all cost values conform to the Cost Mode type
%%    specified	
validate_Xcost_values({struct,L},SrcId,CostMode,VF,VFArgs,ErrorList) ->
	validate_Xcost_values(L,SrcId,CostMode,VF,VFArgs,ErrorList);
validate_Xcost_values([],_,_,_,_,ErrorList) ->
	ErrorList;
validate_Xcost_values([{DstId,_}=Attribute|T],SrcId,CostMode,VF,VFArgs,ErrorList) ->
	%Check the DstId
	NewErrorList = case VF(DstId,VFArgs) of
		false -> 
			[{dst_id_notfound, DstId}] ++ ErrorList;
		true -> 
			ErrorList
	end,
    case metrics:validate_cost_metric(Attribute,SrcId,CostMode) of
			[] -> %% no error found
				validate_Xcost_values(T,SrcId,CostMode,VF,VFArgs,NewErrorList);
			MetricErrorList -> 
				validate_Xcost_values(T,SrcId,CostMode,VF,VFArgs,MetricErrorList ++ NewErrorList)
	end.
