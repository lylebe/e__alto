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

-define(URIMAPTBL, urimaptable). 
-define(COMMONTBLOPTS, [named_table, set, public]).
 
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
		 remove_uri_mapping/1,
		 get_resourceid_for_path/1
]).

%%
%% @doc Performs initialization tasks for this module.
%%
init() ->
	urimap_table_init().

%%
%% @doc Retrieves the IRD.
%%
getIRD() ->
	case registry:get_resource(<<"IRD">>) of
		not_found -> %Initialize IRD;
			updateIRD( {struct,[{<<"meta">>,{struct,[]}},{<<"resources">>,{struct,[]}}]} ),
			getIRD();
		Value -> Value
	end.

%%
%% @doc Replaces the IRD with the new IRD.
%%
updateIRD(IRD) ->
	e_alto_backend:store(<<"IRD">>,{directory, IRD, undefined}).

%%
%% @doc Determines if a resource is in the IRD.
%%
in_ird(Name) ->
	IRD = getIRD(),
	case ej:get({"resources",Name},IRD) of
		undefined -> false;
		_ -> true
	end.

%%
%% @doc Extracts a path from a URI.
%%
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
%% @doc Get the specific version of the resource
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
%% @doc Gets a Resource by retrieving the URI Path and Tag
%%
get_resource_by_path(URI, Tag) ->
	case get_resourceid_for_path(URI) of
		not_found -> not_found;
		{_,ResourceId} -> get_resource(ResourceId,Tag)
	end.
	
%%
%% @doc Gets a Resource by retrieving the URI Path
%%
get_resource_by_path(URI) ->
	get_resource_by_path(URI, undefined).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Registration Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
%% 
%% @doc Determines if a path is registered
%%
is_registered(PathURI) ->
	etscommon:has(?URIMAPTBL,?COMMONTBLOPTS,PathURI).

%% 
%% @doc Gets the Resource ID associated with a path
%%
get_resourceid_for_path(PathURI) ->
	etscommon:get_value(?URIMAPTBL,?COMMONTBLOPTS,PathURI).

%%
%% @doc Adds a mapping URI => Resource to the Registry table.
%%
add_uri_mapping(K,V) when is_list(K) ->
	add_uri_mapping(list_to_binary(K),V);
add_uri_mapping(K,V) ->
	etscommon:set(?URIMAPTBL,?COMMONTBLOPTS,K,V).

%%
%% @doc Deregisters the Reosource from the URI table.
%%	
deregister_mapping(ResourcePath) ->
	deregister(get_resourceid_for_path(ResourcePath)).

%%
%% @doc Deregisters the Value in the URI table.
%%	
deregister(K) ->
	try 
		URIs = ets:match(?URIMAPTBL, {'$1', K}),
		lists:foreach(fun(E) -> ets:delete(?URIMAPTBL,E) end, URIs)
	catch error:badarg ->
		urimap_table_init()
	end.

%%	
%% @doc Lists all URIs in the URI registry.
%%	
show_registry() ->
	try
		ets:tab2list(?URIMAPTBL)
	catch error ->
		urimap_table_init(),
		[]
	end.
	
%%
%% @doc Removes a specific URI Mapping
%%	
remove_uri_mapping(K) -> 
	etscommon:delete(?URIMAPTBL,?COMMONTBLOPTS,K).
	
%%
%% Internal Function that creates the ETS table for holing URI path to 
%% ResourceID mappings
%%
urimap_table_init() ->
	etscommon:init_table(?URIMAPTBL,?COMMONTBLOPTS).
