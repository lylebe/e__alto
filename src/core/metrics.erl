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
-module(metrics).

-export([ validate_cost_metric/3,
		  is_valid_instance/2,
		  bin_to_num/1,
		  metric_to_record/2,
		  metric_to_record/3,
		  metric_to_record/4,
		  metric_to_json/1,
		  metric_to_EJSON/1,
		  autogen_metricname/2,
		  updateIRD/2,
		  removeFromSet/2,
		  addToSet/3,
		  gen_metrics/1,
		  get_metricnames/1,
		  indexOf/1,
		  indexOf/2,
		  addToIndex/6,
		  removeFromIndex/2
		   ]).

-type basetype() :: 'costmap' | 'epcostmap' | 'unknown'.
-type subtype() :: 'fine' | 'coarse' | 'unknown'.

-include("e_alto.hrl").

%% 
%% Validates that the attributes's value conforms to the format (ordinal, numerical)
%% of the metric it represents.
%%	
validate_cost_metric({AttributeName,AttributeValue},JSONPath,CostMode) ->
	case is_valid_instance(CostMode, AttributeValue) of
		false ->
			[{invalid_metric_value, JSONPath ++ <<"/">> ++ AttributeName, AttributeValue}];
		true ->
			[]
	end.

%%
%% Safely changes a binary value to a numeric value
%%
bin_to_num(Bin) when is_list(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

%%
%% Selects which test to perform to validate that the value is of the 
%% specified metric type
%%
is_valid_instance(numerical, Val) -> 
	is_numerical(Val);
is_valid_instance(ordinal,Val) ->
	is_ordinal(Val).

%%
%% Validates ordinal metric values for conformance.
%%
is_ordinal(X) when is_integer(X) andalso X >= 0 ->
	true;
is_ordinal(_) ->
	false.
	
%%	
%% Validates numerical metric values for conformance.
%%	
is_numerical(X) when is_float(X) ->
	true;
is_numerical(X) when is_integer(X) ->
	true;
is_numerical(_) ->
	false.
	
-spec metric_to_record(Name :: binary(),
				Mode :: binary(),
				Metric :: binary(),
				Description :: list() ) -> #costmetric{}.
metric_to_record(Name, Mode, Metric, Description) when is_binary(Name), is_binary(Mode), is_binary(Metric), is_list(Description) ->
	#costmetric{name=Name,mode=Mode,metric=Metric,description=Description}.

-spec metric_to_record(Name :: binary(),
				Mode :: binary(),
				Metric :: binary()) -> #costmetric{}.
metric_to_record(Name, Mode, Metric) when is_binary(Name), is_binary(Mode), is_binary(Metric) ->
	#costmetric{name=Name,mode=Mode,metric=Metric}.	

-spec metric_to_record(Mode :: binary(),
				Metric :: binary()) -> #costmetric{}.
metric_to_record(Mode, Metric) when is_binary(Mode), is_binary(Metric) ->
	metric_to_record(autogen_metricname(Mode,Metric),Mode,Metric).

-spec metric_to_json(Metric :: #costmetric{}) 
				-> list().
metric_to_json(Metric) when is_record(Metric, costmetric) -> 	
	mochijson2:encode( metric_to_EJSON(Metric) ).
	
-spec metric_to_EJSON(Metric :: #costmetric{}) 
				-> tuple().	
metric_to_EJSON(Metric) when is_record(Metric, costmetric) ->
	_List = [ {<<"cost-mode">>, Metric#costmetric.mode},
			{<<"cost-metric">>, Metric#costmetric.metric} ],
	_Description = case Metric#costmetric.description of
		undefined -> [];
		Description -> [{ <<"description">>, Description }]
	end,
	{ Metric#costmetric.name, {struct, _List ++ _Description }}.
	
-spec autogen_metricname ( Mode :: binary(),
						   Metric :: binary() ) -> binary().
autogen_metricname(Mode, Metric) when is_binary(Mode) andalso is_binary(Metric) ->
	_Separator = <<"-">>,
	_Suffix = <<"-autogen">>,
	<< Mode/bitstring, _Separator/bitstring, Metric/bitstring, _Suffix/bitstring >>.
	
-spec gen_metrics(Metrics :: [ #costmetric{} ]) -> [ list() ].
gen_metrics(Metrics) when is_list(Metrics) ->
	lists:fold(fun(E,AccIn) -> [ metric_to_json(E) ] ++ AccIn end, Metrics, []).
	
-spec get_metricnames(Metrics :: [ #costmetric{} ]) -> list().
get_metricnames(Metrics) when is_list(Metrics) ->
	lists:foldl(fun(E,AccIn) -> [ E#costmetric.name ] ++ AccIn end, Metrics, []).

-spec updateIRD(Metric :: #costmetric{},
				IRD :: tuple()) -> list().
updateIRD(Metric, IRD) ->
	_MetricEJSON = metric_to_EJSON(Metric),
	ej:set({"meta","cost-types",element(1,_MetricEJSON)}, IRD, element(2,_MetricEJSON)).		

-spec addToSet(Metric :: #costmetric{},
			   SomeData :: any(),
			   Set :: list()) -> list().
addToSet(Metric, SomeData, Set) ->
	_Key = metric_to_EJSON(Metric),
	lists:keystore(_Key, 1, Set, {_Key, SomeData}).
	
-spec removeFromSet(Metric :: #costmetric{},
					Set :: list()) -> list().
removeFromSet(Metric, Set) -> 
	case lists:keytake(metric_to_EJSON(Metric), 1, Set) of
		false -> Set;
		{_, _, NewSet} -> NewSet
	end.

-spec indexOf(Metric :: #costmetric{}) -> any().

indexOf(Metric) when is_record(Metric,costmetric) ->
	_Key = { index, Metric#costmetric.metric, Metric#costmetric.mode },
	e_alto_backend:get_constant(_Key).

-spec indexOf(Metric :: binary(),
			  Mode :: binary() ) -> any().

indexOf(Metric, Mode) ->
	_Key = { index, Metric, Mode },
	e_alto_backend:get_constant(_Key).	

-spec addToIndex(Metric :: #costmetric{},
				 Type :: basetype(),
				 SubType :: subtype(),
				 ResourceId :: any(),
				 Tag :: any(),
				 SomeData :: any()) -> atom().
				 
addToIndex(Metric, Type, SubType, ResourceId, Tag, SomeData) ->
	_Key = { index, Metric#costmetric.metric, Metric#costmetric.mode },
	_NewValue = case e_alto_backend:get_constant(_Key) of
		not_found -> [{ Type, SubType, ResourceId, Tag, SomeData}];  
		{_, Value} -> [{ Type, SubType, ResourceId, Tag, SomeData}] ++ Value
	end, 	
	e_alto_backend:set_constant(_Key,_NewValue).
	
-spec removeFromIndex(Metric :: #costmetric{},
					  ResourceId :: any()) -> list().
					  
removeFromIndex(Metric, ResourceId) -> 
	_Key = { index, Metric#costmetric.metric, Metric#costmetric.mode },
	case e_alto_backend:get_constant(_Key) of
		not_found -> not_found;
		{_, Value} -> 	
			_NewValue = case lists:keytake(ResourceId, 3, Value) of
				false -> Value;
				{_, _, NewSet} -> NewSet
			end,
			e_alto:set_constant(_Key,_NewValue)
	end.
	
