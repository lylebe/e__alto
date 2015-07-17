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

-export([ 
	init/0, 
	get_item/2, 
	remove/2, 
	store/2, 
	store/3, 
	get_latest_version/1,
	set_resource_info/2,
	get_resource_info/1,
	delete_resource_info/1, 
	find_resource_info/2,
	set_resource_state/3
 ]).
 
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
		
set_resource_info(ResourceId, Status, Info) ->
	_Timestamp = os:system_time(),
	%Extract URI Path
	{ok,{_,_,_,_,_URIPath,_}} = http_uri:parse( ej:get({"uri"},Info) ),
	%Get the ResourceType
	ResourceType = get_resource_type( ej:get({"media-type"}, Info) ),
	_Key = ResourceId ++ atom_to_list(ResourceType),
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
	try
		ets:lookup(?RESOURCETBL, ResourceId)
	catch 
		error:badarg ->
			create_resource_table(),
			not_found
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Removes the Resource Item
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec delete_resource_info(ResourceId :: string()) -> atom().

delete_resource_info(ResourceId) ->
	try
		ets:delete(?RESOURCETBL, ResourceId)
	catch 
		error:badarg ->
			create_resource_table(),
			not_found
	end.	

%%%%%%%%%%%%%%%%%%%%%%%%
%% Intializes Table
%%%%%%%%%%%%%%%%%%%%%%%%
-spec init() -> term().

init() ->
	create_entity_table(),
	create_latest_version_table(),
	create_resource_table().

%%%%%%%%%%%%%%%%%%%%%%%%
%% Retrieves Item
%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_item(ResourceId :: string(),
				Vtag :: string()) -> [{ string(), string(), string(), string(), string() }].

get_item(ResourceId, Vtag) ->
	_Key = lists:concat([ResourceId, Vtag]), 
	try
		RetVal = ets:lookup(?ENTITYTBL, _Key),
		case length(RetVal) of
			0 -> not_found;
			_ -> lists:nth(1,RetVal)
		end
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
				get_item(ResourceId,Vtag)			
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

create_resource_table() ->
	try
		ets:new(?RESOURCETBL, [named_table, set, public])
	catch
		error:badarg -> resource_create_error
	end.
