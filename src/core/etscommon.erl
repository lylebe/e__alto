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
%% @doc A set of common ets functions
%% @end
%%
-module(etscommon).

-export([
		  has/2,
		  has/3,
		  set/3,
		  set/4,
		  get_value/2,
		  get_value/3,
		  delete/2,
		  delete/3,
		  table_init/2,
		  table_exists/1
	]).
	
%%
%% @doc Determines if an entry exists for the key.
%%
has(Tablename,K) ->
	case get_value(Tablename,K) of
		not_found -> false;
		_ -> true
	end.
	
%%
%% @doc Determines if an entry exists for the key.  If there is a 
%% failure it will check to see if the table exists and if not it will 
%% create it.
%%
has(Tablename,InitOptions,K) ->
	try 
		has(Tablename,K)
	catch
		error:badarg ->
			table_init(Tablename,InitOptions),
			not_found	
	end.
		
%%
%% Simple ETS insert operation.
%%	
-spec set(Tablename :: atom(),
				K :: any(),
				V :: any()) -> any().
		
set(Tablename,K,V) -> ets:insert(Tablename, {K,V}).

%%
%% Sets the value in the table for the key provided.  If there is a 
%% failure it will check to see if the table exists and if not it will 
%% create it.
%%
set(Tablename,InitOptions,K,V) ->
	try
		ets:insert(Tablename, {K,V})
	catch error:badarg -> 
		table_init(Tablename,InitOptions),
		ets:insert(Tablename, {K,V})
	end.

%%
%% Simple ETS lookup operation.
%%
get_value(Tablename,K) ->
	_Retval = ets:lookup(Tablename,K),
	case length(_Retval) of
		0 -> not_found;
		_ -> lists:nth(1,_Retval)
	end.
	
%%
%% Retrieves a value from the table.  If there is a failure it will check
%% to see if the table exists and if not it will create it.
%% 
get_value(Tablename,InitOptions,K) ->
	try
		get_value(Tablename,K)
	catch
		error:badarg -> 
			table_init(Tablename,InitOptions),
			not_found	
	end.
	
%%
%% Simple ETS delete operation
%%	
delete(Tablename,K) -> ets:delete(Tablename,K).

%%
%% Deletes an item in the table. If there is a failure it will check
%% to see if the table exists and if not it will create it.
%%
delete(Tablename,InitOptions,K) ->
	try
		ets:delete(Tablename,K)
	catch error:badarg ->
		table_init(Tablename,InitOptions),
		not_found	
	end.

%%
%% Internal Function that creates the ETS table for holing URI path to 
%% ResourceID mappings
%%
table_init(Tablename,Options) ->
	try
		case table_exists(Tablename) of
			false -> 
				ets:new(Tablename, Options),
				lager:info("~p--ETS Table Created--Name = ~p and Options = ~p",[?MODULE, Tablename,Options]);
			true -> {error,already_created}
		end
	catch
		error:badarg -> table_create_error
	end.

%%
%% Determines if the table in question exists in ETS
%%
table_exists(Tablename) ->
  case ets:info(Tablename) of
    undefined -> false;
    _ -> true
end.
