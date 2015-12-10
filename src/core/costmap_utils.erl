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
		true -> { ?FG, 1.0 };
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
							_SeachType = getSearchType(MapPrefix,Constraints),
							{_, _Indices } = metrics:indexOf(CostMetric,CostMode),
							%%Move the map we want to search to the head of the line in the indices
							_Indices1 = case lists:keytake(_CostMapId,3,_Indices) of
								false -> _Indices;
							 	 {value, Key, TupleList2 } -> lists:append([Key],TupleList2)
							end,
							SearchMapType = case MapPrefix of 
								"cost-map" -> costmap;
								"endpoint-cost-map" -> epcostmap
							end,							
							{ _FinalResult, _ } = searchMaps(_Indices1,getSearchType(MapPrefix,Constraints),SearchMapType,{struct,[ { list_to_binary(MapPrefix), {struct, []} } ]},
								[ {ej:get({PathPrefix,"srcs"},Body), ej:get({PathPrefix,"dsts"},Body) } ], Constraints),
	
							_CostMap = registry:get_resource(_CostMapId),
							_PrelimResult = { struct, [{<<"meta">>, {struct,[ {<<"cost-type">>, ej:get({"meta","cost-type"},_CostMap)} ] } },	
										      { list_to_binary(MapPrefix), _FinalResult } ] },
							case SearchMapType of
								costmap ->
									ej:set({<<"meta">>,<<"dependent-vtags">>}, _PrelimResult, ej:get({"meta","dependent-vtags"},_CostMap)); 
							    epcostmap ->
									_PrelimResult
							end
					end
			end;
		{false, SomeErrors } ->
			{error, SomeErrors}
	end. 
	
getSearchType("cost-map", _) ->
	?CG;
getSearchType("endpoint-cost-map", Constraints) ->
	case lists:keyfind(?FG, 1, Constraints) of
		false -> unknown;
		_ -> ?FG
	end.

full_report(List) when is_list(List) ->
	lager:info("Reporting~n~n",[]),
	lists:all(fun(E) -> lager:info("~p~n",[E]), true end, List),
	lager:info("~n",[]).
	
%% Iterates over entries while misses is not empty and we can search more maps
searchMaps(_,_,_,Hits,[],_) ->
	{ Hits, [] };
searchMaps([],_,_,Hits,Misses,_) ->
	{ Hits, Misses };
searchMaps([H|T],SearchType,SearchMapType,Hits,Misses,Constraints) ->
	{ _Hits, _Misses } = searchMaps1(H,SearchType,SearchMapType,Hits,Misses,Constraints),
	searchMaps(T,SearchType,SearchMapType,_Hits,_Misses,Constraints).
	
searchMaps1({MapType,Granularity,ResourceId,Version,_},SearchType,SearchMapType,Hits,Misses,Constraints) ->
	%%Valid Search Conditions
	%%1. Both are fine grained and epcs
	%%2. or non-finegrain
	lager:info("Searching ~p / ~p / ~p",[atom_to_list(MapType),atom_to_list(Granularity),ResourceId]),
	case (((MapType == epcostmap) andalso (SearchMapType == epcostmap) andalso (SearchType == ?FG) and (Granularity == ?FG)) or (SearchType =/= ?FG)) of
		false -> { Hits, Misses };
		true -> case registry:get_resource(ResourceId,Version) of
					not_found -> { Hits, Misses };
					_Map -> 
						%NOTE - This is where criteria unmet is dropped from filter lists
						{_Filters,_} = misses_to_filter(Misses,[],[]),
						case { MapType, SearchMapType } of
						    {costmap, costmap} -> applyFilters( _Filters, Constraints, _Map, {Hits,[]}, <<"cost-map">>,undefined);
						    {epcostmap, epcostmap} -> applyFilters( _Filters, Constraints, _Map, {Hits,[]}, <<"endpoint-cost-map">>,undefined);		
							{epcostmap, costmap} -> %Not a valid search option
								{Hits,Misses};
							{costmap, epcostmap} -> 
								lager:info("Executing costmap search for an epcostmap",[]),
								_NetworkMap = ej:get({"meta","dependent-vtags",1,"resource-id"},_Map),
								_NetworkMapVersion = ej:get({"meta","dependent-vtags",1,"tag"},_Map),
								{ _, Tries } = registry:get_resource(_NetworkMap,_NetworkMapVersion,["appstate"]),
								applyFilters( _Filters, Constraints, _Map, {Hits,[]}, <<"cost-map">>,[{topid,Tries},{returnbase,<<"endpoint-cost-map">>}])
						end
				end
	end.	
	
applyFilters(A,B,C,D,E,undefined) -> 
	applyFilters(A,B,C,D,E,[]);
applyFilters([],_,_,{Hits,Misses},_,_) ->
	{Hits,Misses};
applyFilters(List,Constraints,Map,{Hits,Misses},Prefix,Options) when is_binary(Prefix)->
		applyFilters(List,Constraints,Map,{Hits,Misses},[Prefix],Options);
applyFilters([{SrcFilter,DstFilter}|T],Constraints,Map,{Hits,Misses},Prefix,Options) when is_list(Prefix)->
	{ _Hits, _Misses } = filter2(SrcFilter,DstFilter,Constraints,Map,{Hits,Misses},Prefix,Options),
	applyFilters(T,Constraints,Map,{_Hits,_Misses},Prefix,Options).

sort_list(Structure, BasePath, undefined, Options) ->
	{ struct, _X } = ej:get(BasePath,Structure), 
	{ lists:foldl(fun(E,{AccIn,_Options}) -> case proplists:get_value(keepvalue,_Options,false) of
						false -> {AccIn ++ [element(1,E)],_Options};
						true -> {AccIn ++ [E],_Options}
					   end
					  end,
					  { [], Options },
					  _X),
		[] };
sort_list(Structure, BasePath, List, Options) when is_tuple(BasePath) ->
	sort_list(Structure, tuple_to_list(BasePath), List, Options); 
sort_list(Structure, BasePath, List, undefined) ->
	sort_list(Structure, BasePath, List, []); 
sort_list(Structure, BasePath, List, Options) when is_list(BasePath), is_list(Options) ->
	{ _, _, _Ins, _Outs, _ } = lists:foldl(
		fun(E,{Path,Struct,Ins,Outs,_Options}) -> 
			_X = case proplists:get_value(topid, _Options, undefined) of
				undefined -> ej:get(Path ++ [E], Struct);
				Tries -> case mapservices:getPidForAddress(E,Tries) of
						undefined -> undefined;
						_MappedValue -> {mapped, _MappedValue, ej:get(Path ++ [_MappedValue], Struct)}
					end
			end,
			case _X of
				undefined -> {Path,Struct,Ins,Outs ++ [E],_Options};
				{mapped, _MV, _MVal} -> case proplists:get_value(keepvalue,_Options,false) of
						false -> {Path,Struct,Ins ++ [{mapped, E, _MV}],Outs,_Options};
						true -> {Path,Struct,Ins ++ [{{mapped, E, _MV},_MVal}],Outs,_Options}
					   end;
				Val -> case proplists:get_value(keepvalue,_Options,false) of
						false -> {Path,Struct,Ins ++ [E],Outs,_Options};
						true -> {Path,Struct,Ins ++ [{E,Val}],Outs,_Options}
					   end
			end
		end,
		{ BasePath, Structure, [], [], Options },
		List),
	{_Ins, _Outs}.
	
filter2(RowFilter,ColumnFilter,Constraints,Map,{Hits,Misses},Prefix,Options) when is_tuple(Prefix) ->
	filter2(RowFilter,ColumnFilter,Constraints,Map,{Hits,Misses},tuple_to_list(Prefix),Options);
filter2(RowFilter,ColumnFilter,Constraints,Map,{Hits,Misses},Prefix,Options) -> 
	{RowsPresent,RowsMissing} = sort_list(Map, Prefix, RowFilter, Options),
	% Process the rows that are present and get their values
	{_,_,_,FinalHits,FinalMisses,_,_} = lists:foldl(
		fun(Row, {_ColumnFilter,_Constraints,_Map,_Hits,_Misses,_Prefix,_Options}) ->
			% Get all of the values present in the map
			{ _RowVal, _RowReturnBase } = case Row of
				{mapped, SrcRow, MappedRow} -> { MappedRow, SrcRow };
				NonMappedRow -> { NonMappedRow, NonMappedRow }
			end,
			{ ValuesPresent, ColumnsNotPresent } = sort_list(_Map, _Prefix ++ [_RowVal], _ColumnFilter, Options ++ [keepvalue]),
			% Test those values against the Criteria, if they pass then 
			% add them to the return structure
			_ReturnBase = case proplists:get_value(returnbase, Options, undefined) of
				undefined -> [_Prefix];
				Val -> [Val]
			end,
			{ _, _QueryHits, ConstraintMisses, _ } = lists:foldl(
				fun(E,{_Constraints_, _Hits_, _Misses_, _EJPath}) -> 
					case E of
						{{mapped, _X, _Y}, _Val1} -> case meets_criteria(_Constraints_,_Val1) of
								true -> {_Constraints_, ej:set_p(_EJPath ++ [_X],_Hits_, _Val1), _Misses_, _EJPath };
								false -> {_Constraints_, _Hits_, [E] ++ _Misses_, _EJPath }
							end;
						{_Col,_Val2} -> case meets_criteria(_Constraints_,_Val2) of
							true ->  {_Constraints_, ej:set_p(_EJPath ++ [_Col],_Hits_, _Val2), _Misses_, _EJPath };
							false -> {_Constraints_, _Hits_, [E] ++ _Misses_ ,_EJPath}
							end
					end
				end,
				{ _Constraints, _Hits, [], _ReturnBase ++ [_RowReturnBase] },
				ValuesPresent),
			% Post process other return values
			_Var2 = case length(ColumnsNotPresent) of
			 0 -> [];
			 _ -> [{missing_columns, Row, ColumnsNotPresent}]
			end,
			_Var3 = case length(ConstraintMisses) of
			 0 -> [];
			 _ -> [{criteria_unmet, Row, ConstraintMisses}]
			end,			
			{_ColumnFilter,_Constraints,_Map,_QueryHits, _Var2 ++ _Var3 ++ _Misses, _Prefix, _Options}
		end,
		{ColumnFilter,Constraints,Map,Hits,Misses,Prefix,Options},
		RowsPresent
	),
	_FinalMisses = case length(RowsMissing) of
		0 -> FinalMisses;
		_ -> [{ missing_rows, RowsMissing, ColumnFilter }] ++ FinalMisses
	end,
	{ FinalHits, _FinalMisses }.

misses_to_filter([],StuffICanFilter,StuffICant) ->
	{StuffICanFilter,StuffICant};
misses_to_filter([H|T],StuffICanFilter,StuffICant) ->
	{ _NewValue1, _NewValue2 } = case H of 
		% What should I do here in the spirit of ALTO?
		{ criteria_unmet, _ , _ } -> { StuffICanFilter, [H] ++ StuffICant };
		{ missing_columns, Row, Columns } -> { [{ [valuesToFilter(Row)], valuesToFilter(Columns) }] ++ StuffICanFilter, StuffICant };
		{ missing_rows, Rows, ColumnFilter } ->
			{ [{ valuesToFilter(Rows), ColumnFilter }] ++ StuffICanFilter, StuffICant };
		{ SrcFilter, DstFilter } -> 
			{ [{ SrcFilter, DstFilter }] ++ StuffICanFilter, StuffICant }
	end,
	misses_to_filter(T,_NewValue1,_NewValue2).

valuesToFilter(Values) when is_list(Values) ->
	lists:foldl(fun(E,AccIn)->[valueToFilter(E)]++AccIn end, [], Values);
valuesToFilter(Values) ->
	valueToFilter(Values).

valueToFilter(Value) ->
	case Value of 
		undefined -> [];
		{{mapped, _Key1, _}, _} -> _Key1;
		{_Key2, _} -> _Key2;
		{mapped, _Key3, _} -> _Key3;
		_Val -> _Val
	end.
		
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
		?FG -> true
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
