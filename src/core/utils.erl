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
	apply_attribute_filter_to_list/3,
	apply_attribute_filter/2,
	apply_attribute_filter/3,
	apply_attribute_filter/4
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
	apply_attribute_filter_to_list1(JSONObjectList, lists:usort(AttributeFilter), []).

%%
%% @doc Applies an AttributeFilter to a list of JSONObjects
%% @end
%%
apply_attribute_filter_to_list(JSONObjectList, undefined, _) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, [], _) ->
	JSONObjectList;
apply_attribute_filter_to_list(JSONObjectList, AttributeFilter, CleanIt) when is_list(AttributeFilter) ->
	List = apply_attribute_filter_to_list1(JSONObjectList, lists:usort(AttributeFilter), []),
	case CleanIt of 
		false -> List;
		true -> clean_list(List)
	end.

%%
%% List iteration that applies apply_attribute_filter to each element
%%
apply_attribute_filter_to_list1([], _, AccIn) ->
	AccIn;
apply_attribute_filter_to_list1([H|T], AttributeFilter, AccIn) ->
	apply_attribute_filter_to_list1(T, AttributeFilter, [apply_attribute_filter1(AttributeFilter, H)] ++ AccIn).
		
%%
%% @doc Applies an AttributeFilter to a JSON Object, e.g. { ... },or named 
%% object - "somename" : { ... }
%% @end
%%		
apply_attribute_filter(undefined, JSONObject) ->		
	JSONObject; %% undefined filter
apply_attribute_filter([], JSONObject) ->
	JSONObject; %% empty filter
apply_attribute_filter(Filter, JSONObject) ->
	apply_attribute_filter1(lists:usort(Filter),JSONObject).
	
%% Internal Function	
apply_attribute_filter1(Filter, {struct,_}=JSONObject) ->
	apply_attribute_filter1(Filter, [], JSONObject, []);  
apply_attribute_filter1(Filter, JSONObject) when is_list(JSONObject) ->
	{struct, apply_attribute_filter1(Filter, [], JSONObject, []) };  
apply_attribute_filter1(Filter, {ObjectName,Value}) when is_list(Value) -> 
	%% Object is a named object so add that to the path in order to apply the filter
	{ObjectName, apply_attribute_filter1(Filter, [], Value, [])};
apply_attribute_filter1(Filter, {ObjectName,{struct,_}=Value}) -> 
	%% Object is a named object so add that to the path in order to apply the filter
	{ObjectName, {struct, apply_attribute_filter1(Filter, [], Value, [])}}.

%%
%% @doc Removes empty objects in a list after the filter is applied.
%%
clean_list(FilteredList) when is_list(FilteredList)->
	lists:foldl(fun(E,AccIn) -> 
					case should_clean(E) of
						true -> AccIn;
						false -> [E] ++ AccIn
					end
				end,
				[],
				FilteredList).

should_clean({struct,[]}) ->
	true;  
should_clean({_,[]}) -> 
	true;
should_clean({_,{struct,[]}}) -> 
	true;
should_clean(_) ->
	false.

%%
%% @doc Applies an attribute filter to a JSON Document using the 
%% Base Path to prepend to the filter when searching
%% @end
%%
apply_attribute_filter(Filter, BasePath, JSONObject) ->
	apply_attribute_filter1(lists:usort(Filter), BasePath, JSONObject, []).

apply_attribute_filter(undefined, _, JSONObject,_) ->
	JSONObject;
apply_attribute_filter([], _, JSONObject,_) ->
	JSONObject;
apply_attribute_filter(Filter, BasePath, JSONObject,EmptyListIfClean) ->
	Val = apply_attribute_filter1(lists:usort(Filter), BasePath, JSONObject, []),
	case (EmptyListIfClean andalso should_clean(Val)) of
		false -> Val;
		true -> []
	end.
	
%%
%% Internal function that iterates over the provided FilterList
%% an applies property filters.
%%
apply_attribute_filter1([], _, _, AccIn) ->
	AccIn;
apply_attribute_filter1([H|T], BasePath, JSONObject, AccIn) ->
	case ej:get(BasePath ++ [H],JSONObject) of
		undefined -> apply_attribute_filter1(T, BasePath, JSONObject, AccIn);
		Value -> apply_attribute_filter1(T, BasePath, JSONObject, [{H,Value}] ++ AccIn )
	end.
