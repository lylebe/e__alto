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
	is_valid_filter/4,
	is_registered/3,
	filter_Xcostmap/6,
	generate_path/2,
	generate_path/3
	]).

-include("e_alto.hrl").

%%
%% @doc Determines if BasePath/CostMode/CostMetric is registered as a path
%%
is_registered(BasePath, CostMode, CostMetric) ->
	registry:is_registered(generate_path(BasePath, CostMode, CostMetric)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Determines if the filter is valid.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_valid_filter(Filter, PathPrefix, ValidationFunction, ErrFormatFunction) ->
	case utils:weak_validate_syntax(Filter) of
		{ok, Body} -> 
			_Errors = lists:flatten ([ utils:check_fields([
						{ {"cost-type"},Body,<<"Missing cost-type field in request">> } ]),
					ErrFormatFunction (ValidationFunction(ej:get({PathPrefix,"srcs"},Body)) ),
					ErrFormatFunction (ValidationFunction(ej:get({PathPrefix,"dsts"},Body)) )
					]),
			_ErrorList = lists:foldl(fun(E,AccIn) -> case (E == nothing) of 
														false -> [E] ++ AccIn;
														true -> AccIn
													 end
													end, [], _Errors),	
			case (ej:get({"cost-type"},Body) =/= undefined) of
				true -> 
					case valid_constraints(ej:get({"constraints"},Body),ej:get({"cost-type","cost-mode"},Body),[],[]) of
						{ true, Constraints } -> case length(_ErrorList) of
							0 -> {true, Body, Constraints};
							_ -> {false, _ErrorList}
						end;
						{ false, ConstraintErrors } -> {false, [ErrFormatFunction(ConstraintErrors)] ++ _ErrorList}
					end;
				false -> 
					{ false, _ErrorList }
			end;
		SomethingElse -> 
			lager:info("Filter did not pass weak validation check",[]),
			{false, SomethingElse}
	end.	

%%
%% @doc Validates a list of Constraints
%%
valid_constraints(undefined,_,_,_) ->
	{ true, [] };
valid_constraints(Conditions,Units,AccIn,Errors) when is_binary(Units) ->
	valid_constraints(Conditions,list_to_atom(binary_to_list(Units)),AccIn,Errors);
valid_constraints(Conditions,Units,AccIn,Errors) when is_list(Units) ->
	valid_constraints(Conditions,list_to_atom(Units),AccIn,Errors);
valid_constraints([], _, AccIn,Errors) ->
	case length(Errors) of
		0 -> { true, AccIn };
		_ -> { false, Errors }
	end;
valid_constraints([H|T], Units, AccIn,Errors) ->
	case valid_constraint(H,Units) of
		{false, SomeValue} -> valid_constraints(T,Units,AccIn,[SomeValue]++Errors);	
		{true, Constraint} -> valid_constraints(T,Units,[Constraint]++AccIn,Errors)
	end.

%%
%% @doc Validates an individual Constraint.
%%
valid_constraint(Condition,Units) when is_binary(Condition) ->
	valid_constraint(binary_to_list(Condition), Units);
valid_constraint(Condition,Units) when is_atom(Units) ->
	[_Operator, _Value] = string:tokens(Condition, " "),
	case (_Operator == "finegrain") of 
		true -> { fine_grain, 1.0 };
		false ->
			{_ValType,_NumValue} = to_unit(_Value),
			_ErrorC1 = case lists:member(_Operator, ["gt","lt","ge","le","eq"]) of 
				false -> [ unknown_operator ] ;
				true -> []
			end,
			_Result = case ((_ValType =/= undefined)) of
				false -> {false, [unknown_type] ++ _ErrorC1 };
				true -> 
					case ((Units == numerical) and (_ValType == floattype)) or ((Units == ordinal) and (_ValType == inttype)) of
						true -> 
							case (length(_ErrorC1) > 0) of
								false -> {true, {list_to_atom(_Operator), _NumValue}};
								true -> { false, _ErrorC1 }
							end;
						false -> {false, value_type_mismatch}
					end
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
%% Generates a URI Path based upon the CostMode and CostMetric.
%%
generate_path(CostMode, CostMetric) when is_binary(CostMode) -> 
	generate_path(binary_to_list(CostMode), CostMetric);
generate_path(CostMode, CostMetric) when is_binary(CostMetric) ->
	generate_path(CostMode, binary_to_list(CostMetric));
generate_path(CostMode, CostMetric) when is_list(CostMode) andalso is_list(CostMetric) ->
	CostMode ++ "/" ++ CostMetric.
	
%%
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
filter_Xcostmap(Path, InputParameters, PathPrefix, MapPrefix, ValidationFunction, ErrorFormatFunction) ->
	case is_valid_filter(InputParameters, PathPrefix, ValidationFunction, ErrorFormatFunction) of 
		{true, Body, Constraints} ->
			CostMetric = ej:get({"cost-type","cost-metric"},Body),
			CostMode = ej:get({"cost-type","cost-mode"},Body),
			case get_filterinfo(Path) of
				not_found ->
					lager:info("Error - No filter information found for Path ~p",[Path]),
					{ internal_error, <<"Internal Error - No filter information found for Path">>};
				_FilterInfo ->
					case contains_filterspec(_FilterInfo, CostMetric, CostMode) of
						{false, nothing} ->
							{not_found, <<"The Costmap could not be located for the Cost Metric and Mode requested">>};
						{true, not_found} ->
							{not_found, <<"Although the Filter request is valid the Costmap could not be located">>};
						{true, _CostMapId} ->
							_CostMap = registry:get_resource(_CostMapId),
							{_Hits, _Misses} = filter( ej:get({PathPrefix,"srcs"},Body), 
																ej:get({PathPrefix,"dsts"},Body), 
																Constraints,
																_CostMap,
																{[],[]},
																MapPrefix),
							_FinalResult = case search_coarsegrained(MapPrefix, Constraints) of
								false -> _Hits;
								true -> 
										_X = searchMaps(CostMetric, CostMode, _CostMapId,_Hits,_Misses,Constraints),
										_Hits %% TODO - Add coarse grained searches
							end,
							{ struct, [{<<"meta">>, {struct,[ {<<"dependent-vtags">>, ej:get({"meta","dependent-vtags"},_CostMap)},
															  {<<"cost-type">>, ej:get({"meta","cost-type"},_CostMap)} ] } },	
										   {<<"cost-map">>, _FinalResult}]}
					end
			end;
		{false, SomeErrors } ->
			{error, SomeErrors}
	end. 

searchMaps(CostMetric,CostMode,SearchedMap,CurrentHits,CurrentMisses,Constraints) ->	
	_CandidateMaps = findCandidateMaps(CostMetric,CostMode,SearchedMap).

%% Tech Debt 2$ - When _Resource is found execute search here rather than storing the map.
%% If we finish the search early we don't have to keep iterating
findCandidateMaps(CostMetric,CostMode,SearchedMap) ->
	{_, _Indices } = metrics:indexOf(CostMetric,CostMode),
	lager:info("Index search returned ~p",[_Indices]),
	{_,_Results} = lists:foldl(fun({_MapType,_Granularity,_ResourceId,_Version,_},{_SearchedMap,AccList}=AccIn) ->
					lager:info("Looking for ~p",[binary_to_list(_ResourceId)]),
					case (_SearchedMap == _ResourceId)  of
						true -> AccIn;
						false ->
								lager:info("Looking to add ~p / ~p",[_ResourceId,_Version]),
								case registry:get_resource(_ResourceId,_Version) of
									not_found -> { _SearchedMap, AccList };
									_ -> { _SearchedMap, [{_MapType,_ResourceId}]++AccList }
								 end
					end
				end,
				{ SearchedMap, [] },
				_Indices),
	lager:info("Candidate Map Results are ~p",[_Results]),
	_Results.

filter([],_,_,_,{_Results,_Misses}=ReturnValue,_) ->
	full_report(["Returning",_Results,_Misses]),
	ReturnValue;
filter([H|T],undefined,Constraints,Map,{Hits,Misses},Prefix) ->
	{_Hits,_Misses} = case applyConstraints(Prefix,H,Constraints,Map,[],[]) of
		{[], _ } -> {Hits, [{row,H}] ++ Misses};
		{_Hits2, _Misses2} -> {Hits ++ _Hits2, Misses ++ _Misses2}
	end,
	filter(T,undefined,Constraints,Map,{_Hits,_Misses},Prefix);
filter(undefined,ColumnFilter,Constraints,Map,ReturnValue,Prefix) ->
	filter(getRowNames(Prefix,Map),ColumnFilter,Constraints,Map,ReturnValue,Prefix);
filter([H|T],ColumnFilter,Constraints,Map,{Hits,Misses},Prefix) ->
	{_Hits,_Misses} = filterRow(Prefix,H,ColumnFilter,Constraints,Map,Hits,Misses),
	filter(T,ColumnFilter,Constraints,Map,{_Hits,_Misses},Prefix).

applyConstraints(Prefix,RowName,Constraints,Map,Hits,Misses) ->
	{_, R1, R2} = case ej:get({Prefix,RowName},Map) of
		undefined -> { undefined, Hits, [{row,RowName}] ++ Misses };
		{struct, Value} -> lists:foldl(fun({Dst,Val}=E,{_Constraints, _Hits,_Misses}) -> 
								case meets_criteria(_Constraints,Val) of
								 true -> {_Constraints, [E] ++ _Hits, _Misses};
								 false -> {_Constraints, _Hits, [{RowName,Dst}] ++ _Misses}
								end
							 end,
							 {Constraints, Hits, Misses},
							 Value)
	end,
	{R1,R2}.							

filterRow(_,_,[],_,_,Hits,Misses) ->
	{Hits,Misses};
filterRow(Prefix,RowName,[H|T],Constraints,Map,Hits,Misses) ->
	{_Hits,_Misses} = case getValue(Prefix,RowName,H,Map,false,Constraints) of
		undefined -> { Hits, [{srcdest, RowName, H}] ++ Misses };
		Val ->  { [{H,Val}] ++ Hits, Misses }
	end,
	filterRow(Prefix,RowName,T,Constraints,Map,_Hits,_Misses).

getRow(MapPrefix,Map,Row) ->
	ej:get({MapPrefix,Row},Map).
	
getRowNames(MapPrefix,Map) ->
	case ej:get({MapPrefix},Map) of
		undefined -> [];
		{struct, List} -> lists:foldl(fun({_RowName,_},AccIn) -> [_RowName] ++ AccIn end, [], List)
	end. 

getValue(MapPrefix,Src,Dst) ->
	getValue(MapPrefix,Src,Dst,false).

getValue(MapPrefix,Src,Dst,Criteria) ->
	getValue(MapPrefix,Src,Dst,false,Criteria).

getValue(MapPrefix,Src,Dst,Map,CanSwitch) ->
	case ej:get({MapPrefix,Src,Dst},Map) of
		undefined -> case CanSwitch of 
				true -> getValue(MapPrefix,Dst,Src,Map,false);
				false -> undefined
			end;
		Value -> Value
	end.
	
getValue(MapPrefix,Src,Dst,Map,CanSwitch,Criteria) ->
	_Val = getValue(MapPrefix,Src,Dst,Map,CanSwitch),
	case meets_criteria(Criteria, _Val) of
		true -> _Val;
		false -> undefined
	end.
	
search_coarsegrained("cost-map", _) ->
	false;
search_coarsegrained("endpoint-cost-map", Constraints) ->
	case lists:keyfind(fine_grain, 1, Constraints) of
		false -> true;
		_ -> false
	end.

full_report(List) when is_list(List) ->
	lager:info("Reporting~n~n",[]),
	lists:all(fun(E) -> lager:info("~p~n",[E]), true end, List),
	lager:info("~n",[]).
	
meets_criteria(_, undefined) ->
	false;
meets_criteria([], _) ->
	true;
meets_criteria([{Operator,Discriminator}|T], Value) ->
	_TestResult = 	case Operator of
		eq -> Value == Discriminator;
		le -> Value =< Discriminator;
		ge -> Value >= Discriminator;
		ne -> Value =/= Discriminator;
		lt -> Value < Discriminator;
		gt -> Value > Discriminator;
		fine_grain -> true
	end,
	case _TestResult of
		false -> false;
		true -> meets_criteria(T,Value)
	end.

get_filterinfo(Path) when is_binary(Path) ->
	{_,Spec}=e_alto_backend:get_constant(<< Path/bitstring, << ?FILTEREXT >>/ bitstring >>),
	lager:info("Spec is ~p",[Spec]),
	Spec;
get_filterinfo(Path) when is_list(Path) ->
	{_,Spec}=e_alto_backend:get_constant(list_to_binary(Path ++ ?FILTEREXT)),
	lager:info("Spec is ~p",[Spec]),
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
