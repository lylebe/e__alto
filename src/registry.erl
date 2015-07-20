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
%% @doc
%% Internally, paths of URIs are mapped to costmaps and network maps.
%%  
%% When a CostMap is added to the system a path on the server
%% is auto generated of the form 
%%
%%  	{Path of URI used to POST}/{CostMode}/{CostType}
%%
%% If a URI was supplied when the CostMap is registered it is stored
%% in the backend using the URI otherwise the generated path is used.
%%
%% When the resource is added 
%% a. the base path is added to a Map URI Table (Path => ResourceID)
%%	(if it was supplied)
%% b. the generated path is added to the CostMap URI Table (always)
%%
%% This module supports CostMap Versions
%%
%% The CostMap Filter Service API allows adding cost maps IF
%% it uses the same network-map in its dependent vtags.
%% Add it generates an alias mapping.  
%%
%% If a costmap with a different base path already exists but 
%% another should replace it, then a force map option is available.
%% @end
%% 
-module(registry).

-define(URIMAPTBL, costmapsurimap). 
 
-export([init/0,

		 getIRD/0,
		 updateIRD/1,
		 in_ird/1,
	
		 extract_path/1,
		
		 get_resource/1, 
		 get_resource/2, 
		 
		 get_resource_by_path/1,
		 get_resource_by_path/2,

		 is_registered/1,
		 deregister_mapping/1,
		 deregister/1,	
		 show_registry/0,

		 add_uri_mapping/2,		
		 remove_uri_mapping/2,
		 get_id_for_path/1
]).

%%
%% @doc Performs initialization tasks for this module.
%%
init() ->
	urimap_table_init().

getIRD() ->
	case registry:get_resource(<<"IRD">>) of
		not_found -> %Initialize IRD;
			updateIRD( {struct,[{<<"meta">>,{struct,[]}},{<<"resources">>,{struct,[]}}]} ),
			getIRD();
		Value -> Value
	end.

updateIRD(IRD) ->
	e_alto_backend:store(<<"IRD">>,{directory, IRD, undefined}).

in_ird(Name) ->
	IRD = getIRD(),
	case ej:get({"resources",Name},IRD) of
		undefined -> false;
		_ -> true
	end.

	
extract_path(URI) when is_binary(URI) ->
	extract_path(binary_to_list(URI));
extract_path(URI) ->
	case http_uri:parse(URI) of
		{ok,{_,_,_,_,Path,_}} -> Path;
		_ -> undefined
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic Read / Query Operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Retrieves the latest version of the specified Cost Map.
%%
get_resource(ResourceId) ->
	get_resource(ResourceId, undefined).

%%
%% @doc Get the specific version of the map
%%
get_resource(ResourceId, Vtag) ->
	Retval = case Vtag of 
		undefined ->
			e_alto_backend:get_latest_version(ResourceId);
		_ ->
			e_alto_backend:get_item(ResourceId,Vtag)
	end,
	case Retval of
		{_, _, _, { _, Resource, _ }, _ } -> Resource;
		_ -> not_found
	end.	
	
%%
%% @doc Gets a CostMap by retrieving the URI Path and Tag
%%
get_resource_by_path(URI, Tag) ->
	try 
		_Retval = ets:lookup(?URIMAPTBL,URI),
		case length(_Retval) of
			0 -> not_found;
			_ -> get_resource(lists:nth(1,_Retval), Tag)
		end
	catch
		error:badarg -> 
			case ets_table_exists(?URIMAPTBL) of
				false ->
					urimap_table_init(),
					not_found;
				true ->
					internal_error
			end
	end.
	
%%
%% @doc Gets a CostMap by retrieving the URI Path
%%
get_resource_by_path(URI) ->
	get_resource_by_path(URI, undefined).
		
%% 
%% @doc Determines if a path is registered
%%
is_registered(PathURI) ->
	case get_id_for_path(PathURI) of
		not_found -> false;
		_ -> true
	end.

%% 
%% @doc Gets the Resource ID associated with a path
%%
get_id_for_path(PathURI) ->
	try 
		_Retval = ets:lookup(?URIMAPTBL, PathURI),
		case length(_Retval) of
			0 -> not_found;
			_ -> lists:nth(1,_Retval)
		end
	catch
		error:badarg -> urimap_table_init()
	end.

%%%%%%%%%%%%%%%%%%%%%
%% Registration
%%%%%%%%%%%%%%%%%%%%%
add_uri_mapping(K,V) ->
	try
		ets:insert(?URIMAPTBL, {K,V})
	catch error:badarg -> 
		urimap_table_init(),
		ets:insert(?URIMAPTBL, {K,V})
	end.
		
deregister_mapping(ResourcePath) ->
	deregister(get_id_for_path(ResourcePath)).
	
deregister(K) ->
	try 
		URIs = ets:match(?URIMAPTBL, {'$1', K}),
		lists:foreach(fun(E) -> ets:delete(?URIMAPTBL,E) end, URIs)
	catch error:badarg ->
		urimap_table_init()
	end.
	
show_registry() ->
	try
		ets:tab2list(?URIMAPTBL)
	catch error ->
		urimap_table_init(),
		[]
	end.
	
remove_uri_mapping(K,V) ->
	try
		ets:delete(?URIMAPTBL,{K,V})
	catch error:badarg ->
		urimap_table_init()
	end.
	
%%
%% Internal Function that creates the ETS table for holing URI path to 
%% ResourceID mappings
%%
urimap_table_init() ->
	try
		case ets_table_exists(?URIMAPTBL) of
			false -> ets:new(?URIMAPTBL, [named_table, set, public]);
			true -> {error,already_created}
		end
	catch
		error:badarg -> table_create_error
	end.

%%
%% Determines if the table in question exists in ETS
%%
ets_table_exists(TableName) ->
  case ets:info(TableName) of
    undefined -> false;
    _ -> true
end.
