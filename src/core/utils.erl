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
%% @doc Generic utilities used across the various modules
%%
%% @end
-module(utils).

-export([
	apply_attribute_filter_to_list/2,
	apply_attribute_filter/2,
	apply_attribute_filter/3
	]).	
	
%%
%% @doc Applies an AttributeFilter to a list of JSONObjects
%% @end
%%
apply_attribute_filter_to_list(JSONObjectList, undefined) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, []) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, AttributeFilter) when is_list(AttributeFilter) ->
	apply_attribute_filter_to_list(JSONObjectList, AttributeFilter, []).

%%
%% List iteration that applies apply_attribute_filter to each element
%%
apply_attribute_filter_to_list([], _, AccIn) ->
	AccIn;
apply_attribute_filter_to_list([H|T], AttributeFilter, AccIn) ->
	apply_attribute_filter_to_list(T, AttributeFilter, [apply_attribute_filter(AttributeFilter, H)] ++ AccIn).
		
%%
%% @doc Applies an AttributeFilter to a JSON Object, e.g. { ... },or named 
%% object - "somename" : { ... }
%% @end
%%		
apply_attribute_filter(undefined, JSONObject) ->		
	JSONObject; %% undefined filter
apply_attribute_filter([], JSONObject) ->
	JSONObject; %% empty filter
apply_attribute_filter(Filter, {struct,L}=JSONObject) ->
	%% Object is normal JSON Object so apply accordingly - no prepended path to search
	{struct, apply_attribute_filter(Filter, [], JSONObject, []) };  
apply_attribute_filter(Filter, {ObjectName,_}=JSONObject) -> 
	%% Object is a named object so add that to the path in order to apply the filter
	{ObjectName, {struct, apply_attribute_filter(Filter, [ObjectName], JSONObject, [])}}.

%%
%% @doc Applies an attribute filter to a JSON Document using the 
%% Base Path to prepend to the filter when searching
%% @end
%%
apply_attribute_filter(Filter, BasePath, JSONObject) ->
	apply_attribute_filter(Filter, BasePath, JSONObject, []).

%%
%% Internal function that iterates over the provided FilterList
%% an applies property filters.
%%
apply_attribute_filter([], _, _, AccIn) ->
	AccIn;
apply_attribute_filter([H|T], BasePath, JSONObject, AccIn) ->
	case ej:get(BasePath ++ [H],JSONObject) of
		undefined -> apply_attribute_filter(T, BasePath, JSONObject, AccIn);
		Value -> apply_attribute_filter(T, BasePath, JSONObject, ej:set({H},JSONObject,Value) )
	end.
