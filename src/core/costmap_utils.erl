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

-define(FILTEREXT, "filterinfo").

-export([	
	validate_Xcostmap/4,
	is_valid_filter/3,
	filter_Xcostmap/4
	]).

-include("e_alto.hrl").

%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Determines if the filter requested is valid.
%%
is_valid_filter(Filter, PathPrefix, ValidationFunction) ->
	case weak_validate_syntax(Filter) of
		{ok, Body} -> 
			case (ej:get({"cost-type"},Body) =/= undefined) of
				true -> 
					{_ConstraintsOk, _ConstraintsValue} = case valid_constraints(ej:get({"constraints"},Body),ej:get({"cost-type","cost-mode"},Body),[]) of
						{ true, empty_list } -> {true, []};
						{ true, Constraints } -> {true, Constraints};
						{ false, Something } -> {false, Something}
					end,
					_SourceErrors = ValidationFunction(ej:get({PathPrefix,"srcs"},Body)),
					_DestErrors = ValidationFunction(ej:get({PathPrefix,"dsts"},Body)),
					case ((length(_SourceErrors) == 0) andalso (length(_DestErrors) == 0) andalso (_ConstraintsOk == true)) of
						false -> 
							lager:info("~p--Is Valid Filter-Syntax validation failed with Constraint Errors = ~p, Source Errors = ~p and Destination Errors = ~p",[?MODULE, _ConstraintsValue, _SourceErrors, _DestErrors]),
							{false, _ConstraintsValue, _SourceErrors, _DestErrors};
						true ->
							lager:info("~p--Is Valid Filter-Syntax validation passed",[?MODULE]),
							{true, Body, _ConstraintsValue}
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
%% funciton only supports numeric and ordinal units at this time.
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
	
%%
%% @doc Retrieves Information based upon the JSON provided request.  
%% 	
filter_Xcostmap(Path, InputParameters, PathPrefix, ValidationFunction) ->
	case is_valid_filter(InputParameters, PathPrefix, ValidationFunction) of 
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
										   {<<"cost-map">>, filter_sources( ej:get({PathPrefix,"srcs"},Body), 
																			ej:get({PathPrefix,"dsts"},Body), 
																			Constraints,
																			_CostMap,
																			[]) }]}
					end
			end;
		{false, ConstraintErrors, SrcErrors, DstErrors} ->
			{error, ConstraintErrors, SrcErrors, DstErrors} 
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
	
meets_criteria([], _) ->
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

get_filterinfo(Path) when is_binary(Path) ->
	{_,Spec}=e_alto_backend:get_constant(<< Path/bitstring, << ?FILTEREXT >>/ bitstring >>),
	Spec;
get_filterinfo(Path) when is_list(Path) ->
	{_,Spec}=e_alto_backend:get_constant(list_to_binary(Path ++ ?FILTEREXT)),
	Spec.

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
