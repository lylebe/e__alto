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
-define(RESOURCETBL, resources).
-define(CONSTTBL, constants).
-define(COMMONTBLOPTS, [named_table, set, public]).

-export([ 
	init/0, 
	get_item/2, 
	get_item_versions/1,
	remove/2, 
	remove_all/1,
	store/2, 
	store/3, 
	get_latest_version/1,
	set_resource_info/2,
	get_resource_info/1,
	delete_resource_info/1, 
	find_resource_info/2,
	set_resource_state/3,
	refresh_latest_version/1,	
	set_constant/2,
	get_constant/1,
	delete_constant/1,
	add_to_set/2,
	remove_from_set/2
 ]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Intializes Tables
%%%%%%%%%%%%%%%%%%%%%%%%
-spec init() -> term().

init() ->
	create_constant_table(),
	create_entity_table(),
	create_latest_version_table(),
	create_resource_table().

set_constant(K,V) -> etscommon:set(?CONSTTBL,?COMMONTBLOPTS,K,V).
get_constant(K) -> etscommon:get_value(?CONSTTBL,?COMMONTBLOPTS,K).
delete_constant(K) -> etscommon:delete(?CONSTTBL,?COMMONTBLOPTS,K).
add_to_set(K,V) ->
	_NewVal = case get_constant(K) of
		not_found -> [K];
		_Val -> [K] ++ _Val
	end,
	set_constant(K,_NewVal).
remove_from_set(K,V) ->
	set_constant(K, lists:delete(V)).
	 
	
	
 
get_resource_type(ApplicationString) ->
	MediaTypes = [ { "application/alto-networkmap+json", networkmap },
				   { "application/alto-costmap+json", costmap },
				   { "application/alto-directory+json", directory },
				   { "application/alto-endpointprop+json", endpointprops },
				   { "application/alto-endpointcost+json", endpointcosts },
				   { "application/alto-networkmapfilter+json", filterednetworkmap },
				   { "application/alto-costmapfilter+json", filteredcostmap }],
	proplists:get_value(ApplicationString, MediaTypes). 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sets the Resource State (active, inactive, ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_resource_state(  URIPath :: string(),
							MediaType :: string(),
							State :: atom() ) ->	atom().
 
set_resource_state(URIPath, MediaType, State) ->
	_Resource = find_resource_info(URIPath, MediaType),
	case length(_Resource) of
		0 -> not_found;
		1 -> 
			_NewRec = erlang:setelement(3, lists:nth(1), State),
			try
				ets:insert(?RESOURCETBL, _NewRec)
			catch
				error:badarg ->
					create_resource_table(),
					ets:insert(?RESOURCETBL, _NewRec)
			end
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gets the Resource Information based upon the URI Path and Media Type provided.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec find_resource_info( URIPath :: string(),
							MediaType :: string() ) ->	[ any() ].
 
find_resource_info(URIPath, MediaType) ->
	_Type = get_resource_type(MediaType),
	ets:match_object(?RESOURCETBL, { URIPath ++ atom_to_list(_Type), '_', '_', '_', '_', '_' }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sets the Resource Information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_resource_info( ResourceId :: string(),
		Info :: tuple() ) -> boolean().
		
set_resource_info(ResourceId, Info) ->	
	set_resource_info(ResourceId, inactive, Info).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sets the Resource Information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_resource_info( ResourceId :: string(),
		Status :: atom(),
		Info :: tuple() ) -> boolean().
		
set_resource_info(ResourceId, Status, Info) when is_binary(ResourceId) ->
	_Timestamp = list_to_binary( io_lib:format("~p~p~p", tuple_to_list(erlang:now()) )),
	%Extract URI Path
	{ok,{_,_,_,_,_URIPath,_}} = http_uri:parse( ej:get({"uri"},Info) ),
	%Get the ResourceType
	ResourceType = get_resource_type( ej:get({"media-type"}, Info) ),
	_X = list_to_binary(atom_to_list(ResourceType)),
	_Key = <<ResourceId/binary, _X/binary>>,
	case ResourceType of 
		undefined -> not_found;
		_ ->
		try
			ets:insert(?RESOURCETBL, { _Key, ResourceId, Status, ResourceType, _URIPath, Info, _Timestamp })
		catch
			error:badarg ->
				create_resource_table(),
				ets:insert(?RESOURCETBL, { _Key, ResourceId, Status, ResourceType, _URIPath, Info, _Timestamp })
		end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Retrieves the Resource Information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_resource_info(ResourceId :: string()) -> { string(), string(), string(), string() }.

get_resource_info(ResourceId) -> 
	etscommon:get_value(?RESOURCETBL,?COMMONTBLOPTS,ResourceId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Removes the Resource Item
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec delete_resource_info(ResourceId :: string()) -> atom().

delete_resource_info(ResourceId) ->
	etscommon:delete(?RESOURCETBL,?COMMONTBLOPTS,ResourceId).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Retrieve All Versions of an Item
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_item_versions(ResourceId :: string()) -> 
	[{ string(), string(), string(), string(), string() }].

get_item_versions(ResourceId) -> 
	try
		ets:match_object(?ENTITYTBL, {'_', ResourceId, '_', '_', '_'})
	catch 
		error:badarg ->
			create_entity_table(),
			not_found
	end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Retrieves Item
%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_item(ResourceId :: string(),
				Vtag :: string()) -> [{ string(), string(), string(), string(), string() }].

get_item(ResourceId, Vtag) when is_binary(ResourceId) andalso is_binary(Vtag) ->
	_Key = <<ResourceId/bitstring, Vtag/bitstring>>, 
	etscommon:get_value(?ENTITYTBL,?COMMONTBLOPTS,_Key).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Removes Item
%%%%%%%%%%%%%%%%%%%%%%%%
-spec remove(ResourceId :: string(),
				Vtag :: string()) -> atom().

remove(ResourceId, Vtag) when is_binary(ResourceId) andalso is_binary(Vtag) ->
	_Key = <<ResourceId/bitstring, Vtag/bitstring>>,
	etscommon:delete(?ENTITYTBL,?COMMONTBLOPTS,_Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Removes all instances of an item
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec remove_all(ResourceId :: string()) -> atom().

remove_all(ResourceId) ->	
	try
		_Results = ets:match(?ENTITYTBL,{'$1', ResourceId, '_', '_', '_' }),
		lists:foreach(fun([E]) -> 
						ets:delete(?ENTITYTBL,E)
					   end,
					   _Results),
		 ets:delete(?LATESTVSNTBL, ResourceId)
	catch
		error:badarg -> 
			create_entity_table(),
			create_latest_version_table()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Stores Item
%%%%%%%%%%%%%%%%%%%%%%%%
-spec store( ResourceId :: string(),
			 Data :: string() ) -> boolean().

store(ResourceId, Data) ->	
	store(ResourceId, integer_to_binary(uuid:get_v1_time()) , Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stores Item (with vtag)
%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec store( ResourceId :: binary(),
			 Vtag :: binary(),
			 Data :: any() ) -> boolean().

store(ResourceId, Vtag, Data) when is_binary(ResourceId) andalso is_binary(Vtag) ->
	_Key = <<ResourceId/bitstring, Vtag/bitstring>>,
	_Timestamp = list_to_binary( io_lib:format("~p~p~p", tuple_to_list(erlang:now()) )),
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
				get_item(ResourceId,Vtag)			
		end
	catch
		error:badarg -> 
			create_entity_table(),
			create_latest_version_table()
	end.			
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Finds the latest version and sets it
%% Usually peformed after a delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
refresh_latest_version(ResourceId) ->	
	try
		_Results = ets:match(?ENTITYTBL,{'_', ResourceId, '$1', '_', '$2' }),
		case length(_Results) of
			0 ->  %%Erase 
				ets:delete(?LATESTVSNTBL, ResourceId);
			_ ->
				_Sorted = lists:keysort(2,_Results),
				{_LatestVtag, _LatestTimestamp} = lists:last(_Results),
				ets:insert(?LATESTVSNTBL, {ResourceId, _LatestVtag, _LatestTimestamp})	
		end
	catch
		error:badarg -> 
			create_entity_table(),
			create_latest_version_table()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ETS Table Creation - Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_constant_table() -> etscommon:table_init(?CONSTTBL,?COMMONTBLOPTS).
create_entity_table() -> etscommon:table_init(?ENTITYTBL,?COMMONTBLOPTS).
create_latest_version_table() -> etscommon:table_init(?LATESTVSNTBL,?COMMONTBLOPTS).
create_resource_table() -> etscommon:table_init(?RESOURCETBL,?COMMONTBLOPTS).
