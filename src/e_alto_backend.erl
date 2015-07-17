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
%% @doc Extensions of the ej module (http://github.com/set/ej) to provide
%% support for JSON Merge Patch (RFC 7396) and JSON Patch (RFC 6902).
%%
%% @end
-module(e_alto_backend).

-define(ENTITYTBL, entitytable).
-define(LATESTVSNTBL, latestvertable).

-export([ 
	init/0, 
	get_item/2, 
	remove/2, 
	store/2, 
	store/3, 
	get_latest_version/1 
 ]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Intializes Table
%%%%%%%%%%%%%%%%%%%%%%%%
-spec init() -> term().

init() ->
	create_entity_table(),
	create_latest_version_table().

%%%%%%%%%%%%%%%%%%%%%%%%
%% Retrieves Item
%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_item(ResourceId :: string(),
				Vtag :: string()) -> { string(), string(), string(), string(), string() }.

get_item(ResourceId, Vtag) ->
	_Key = lists:concat([ResourceId, Vtag]), 
	try
		ets:lookup(?ENTITYTBL, _Key)
	catch 
		error:badarg ->
			create_entity_table(),
			not_found
	end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Removes Item
%%%%%%%%%%%%%%%%%%%%%%%%
-spec remove(ResourceId :: string(),
				Vtag :: string()) -> atom().

remove(ResourceId, Vtag) ->
	_Key = lists:concat([ResourceId, Vtag]),
	try
		ets:delete(?ENTITYTBL, _Key)
	catch 
		error:badarg ->
			create_entity_table(),
			not_found
	end.	

%%%%%%%%%%%%%%%%%%%%%%%%
%% Stores Item
%%%%%%%%%%%%%%%%%%%%%%%%
-spec store( ResourceId :: string(),
			 Data :: string() ) -> boolean().

store(ResourceId, Data) ->	
	store(ResourceId, uuid:new(os:getpid(), 'erlang'), Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stores Item (with vtag)
%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec store( ResourceId :: string(),
			 Vtag :: string(),
			 Data :: string() ) -> boolean().

store(ResourceId, Vtag, Data) ->
	_Key = list:concat([ResourceId, Vtag]),
	_Timestamp = os:system_time(),
	try
		ets:insert(?ENTITYTBL, {_Key, ResourceId, Vtag, Data, _Timestamp }),
		ets:insert(?LATESTVSNTBL, {ResourceId, Vtag, _Timestamp})
	catch
		error:badarg ->
			create_entity_table(),
			create_latest_version_table(),
			ets:insert(?ENTITYTBL, {_Key, ResourceId, Vtag, Data, _Timestamp }),
			ets:insert(?LATESTVSNTBL, {ResourceId, Vtag, _Timestamp})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gets the latest version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_latest_version( 
	ResourceId :: string() 
   ) -> { string(), string(), string(), string(), string() }.

get_latest_version(ResourceId) ->
	try 
		VersionValue = ets:lookup(?LATESTVSNTBL, ResourceId),
		case length(VersionValue) of
			0 ->
				not_found;
			_ -> 
				[ {_, Vtag, _} | _ ] = VersionValue,
				_Key = lists:concat([ResourceId, Vtag]),
				ets:lookup(?ENTITYTBL, _Key)				
		end
	catch
		error:badarg -> 
			create_entity_table(),
			create_latest_version_table()
	end.			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ETS Table Creation - Private Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_entity_table() ->
	try
		ets:new(?ENTITYTBL, [named_table, set, public])
	catch
		error:badarg -> entity_table_create_error
	end.
	
create_latest_version_table() ->
	try
		ets:new(?LATESTVSNTBL, [named_table, set, public])
	catch
		error:badarg -> latest_version_create_error
	end.
