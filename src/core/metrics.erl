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
		  bin_to_num/1 ]).

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
%% Selects which test to perform to validate that the valus is of the 
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
