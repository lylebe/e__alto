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
%% @doc ALTO (RFC 7285) Cost Map Services functions (including Filtered 
%% Cost Maps).  This module also supports Constraints.
%%
%% @end
-module(costmapservices).

-export([init/0,
		 get_costmap/1, 
		 get_costmap/2, 
		 
		 get_costmap_by_path/1,
		 get_costmap_by_path/2,
		 get_costmap_by_path/3,
		 get_costmap_by_path/4,

		 is_registered/1,
		 is_registered/3,

		 get_id_for_path/1,
		 get_id_for_path/3,
		 
		 get_costmap_by_filter/2, 
		 get_costmap_by_filter/3,

		 register_mapping/3,
		 register_mapping/4,
		 
		 deregister_mapping/1,
		 deregister_mapping/3,
		 
		 store_costmap/3,
		 %store_filtered_costmap/2,
		 
		 delete_costmap/2
		 %delete_filtered_costmap/2
		]).

-include("e_alto.hrl").

-define(URIMAPTBL, costmapsurimap).
-define(COSTFILTERSVCTBL, costfiltersvc).


generate_metainfo(CostName,CostMode,CostMetric,MapId) ->
	generate_metainfo(CostName++"-cost-map",CostMode,CostMetric,"",MapId,generate_path("", CostMode, CostMetric)).

generate_metainfo(CostName,CostMode,CostMetric,Description,MapId,Path) -> 
	_Part1 = "\"meta\" : { \"cost-types\" : { \"" ++ CostName ++ "\" : {
		\"cost-mode\":\"" ++ CostMode ++ "\"
		\"cost-metric\"" ++ CostMetric ++ "\"",
	_Part2 = case is_list(Description) of 
			true -> Description;
			false -> ""
	end,
	_MetaPart = _Part1 ++ _Part2 ++ "} } },",
	_Part4 = "\"resources\":{ \"" ++ CostName ++ "\" : {
		\"uri\"" ++ application:get_env(?APPLICATIONNAME, uri_base, "http://localhost") ++ Path ++ "\"\,
		\"media-type\" : \"application/alto-costmap+json\",
		\"capabilities\" : {
			\"cost-type-names\" : [ \"" ++ CostName ++ "\" ]
		},
		\"uses\": [ \"" ++ MapId ++ "\" ] } }",
	"{" ++ _MetaPart ++ _Part4 ++ "}".	

%%
%% @doc Stores the Cost Map information.  
%%
%% Name is the resource name for the CostMap 
%%
%% IRD represents the IRD associated with the CostMap
%%
%% CostMap - the actual CostMap data
%% @end
%% 
store_costmap(Name, IRD, CostMap) ->
	%% Step 1 - Register - Just Do it even if it overwrites stuff
	%% this is a write command not an update so deal with the overwrite
	case ej:get({"resources",Name},IRD) of
		undefined -> not_found;
		_ResourceEntry ->
			_BasePath =
			try
				case http_uri:parse(ej:get({"uri"}, _ResourceEntry)) of
					{ok,{_,_,_,Path,_}} -> 
					Path;
					_ -> "/"
				end
			catch 
				error ->
					"/"
			end,
			_CostName = ej:get({"capabilities","cost-type-names"},_ResourceEntry),
			_CostMode = binary_to_list(ej:get({"meta",_CostName,"cost-mode"},_ResourceEntry)),
			_CostMetric = ej:get({"meta",_CostName,"cost-metric"},_ResourceEntry),
			register_mapping(Name,_BasePath,_CostMode,_CostMetric)
	end,
			
	%% Step 2 - Add the map under its resource name
	e_alto_backend:store(Name, {costmap ,CostMap,nostate}).


delete_costmap(BasePath, Options) ->
	%% Options will include atoms such as latest_version or all_versions
	ok.
	
delete_costmap(BasePath) ->
	%% Delete All Versions - guess so....
	ok.

%%
%% Internally, paths of URIs are mapped to costmaps.
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
%% a. the base path is added to a CostMap URI Table (Path => ResourceID)
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
%% 
 
%%
%% @doc Performs initialization tasks for this module.
%%
init() ->
	urimap_table_init().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic Read / Query Operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Retrieves the latest version of the specified Cost Map.
%%
get_costmap(MapIdentifier) ->
	get_costmap(MapIdentifier, undefined).

%%
%% @doc Get the specific version of the map
%%
get_costmap(MapIdentifier, Vtag) ->
	Retval = case Vtag of 
		undefined ->
			e_alto_backend:get_lastest_version(MapIdentifier);
		_ ->
			e_alto_backend:get_item(MapIdentifier,Vtag)
	end,
	case length(Retval) of
		{_, _, _, { _, CostMap, _ }, _ } -> CostMap;
		_ -> not_found
	end.	
	
%%
%% @doc Gets a CostMap by retrieving the URI Path and Tag
%%
get_costmap_by_path(URI, Tag) ->
	try 
		_Retval = ets:lookup(?URIMAPTBL,URI),
		case length(_Retval) of
			0 -> not_found;
			_ -> get_costmap(lists:nth(1,_Retval), Tag)
		end
	catch
		error:badarg -> urimap_table_init()
	end.
	
%%
%% @doc Gets a CostMap by retrieving the URI Path
%%
get_costmap_by_path(URI) ->
	get_costmap_by_path(URI, undefined).
	
%%
%% @doc Gets a CostMap by retrieving the generated URI Path
%%
get_costmap_by_path(BasePath,CostMode, CostMetric) ->
	get_costmap_by_path(generate_path(BasePath, CostMode, CostMetric)).

%%
%% @doc Gets a CostMap by retrieving the generated URI Path and Tag
%%
get_costmap_by_path(BasePath,CostMode, CostMetric, Tag) ->
	get_costmap_by_path(generate_path(BasePath, CostMode, CostMetric),Tag).
	
%% 
%% @doc Determines if a path is registered
%%
is_registered(PathURI) ->
	case get_id_for_path(PathURI) of
		not_found -> false;
		_ -> true
	end.
	
%%
%% @doc Determines if BasePath/CostMode/CostMetric is registered as a path
%%
is_registered(BasePath, CostMode, CostMetric) ->
	is_registered(generate_path(BasePath, CostMode, CostMetric)).

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
	
%%
%% @doc Gets the Resource ID for the BasePath/CostMode/CostMetric path
%%
get_id_for_path(BasePath, CostMode, CostMetric) ->
	get_id_for_path(generate_path(BasePath, CostMode, CostMetric)).
	
%%
%% INTERNAL FUNCTION
%% Generates a URI Path based upon the CostMode and CostMetric.
%%
generate_path(CostMode, CostMetric) when is_binary(CostMode) -> 
	generate_path(binary_to_list(CostMode), CostMetric);
generate_path(CostMode, CostMetric) when is_binary(CostMetric) ->
	generate_path(CostMode, binary_to_list(CostMetric));
generate_path(CostMode, CostMetric) when is_list(CostMode) andalso is_list(CostMetric) ->
	CostMode ++ "/" ++ CostMetric.
	
%%
%% INTERNAL FUNCTION
%% Generates a Path given the base path and metric information
%%
generate_path(BasePath, CostMode, CostMetric) ->
		BasePath ++ "/" ++ generate_path(CostMode, CostMetric).	

%%%%%%%%%%%%%%%%%%%%
%% Filtering
%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Retrieves Pids based upon the JSON provided request.  
%% 
get_costmap_by_filter(MapIdentifier, InputParameters) ->
	filter_costmap( mapservices:get_map(MapIdentifier), InputParameters ).

%%
%% @doc Retrieves Pids based upon the JSON provided request.  
%% 
get_costmap_by_filter(MapIdentifier, Vtag, InputParameters) ->
	filter_costmap( mapservices:get_map(MapIdentifier, Vtag), InputParameters ).	
	
filter_costmap(not_found, _) ->
	not_found;
filter_costmap(CostMap, InputParameters) ->
	A = { struct, [{<<"meta">>, {struct, []}},	
				   {<<"network-map">>, filter_pids( ej:get({<<"pids">>},InputParameters), 
													 CostMap, 
													 ej:get({<<"address-types">>},InputParameters) 
													 ) }]},	
	ej:set({<<"meta">>,<<"vtag">>}, A, ej:get({<<"meta">>,<<"vtag">>},CostMap)).
	
	
filter_pids([], NetworkMap, AddressTypeFilter) ->
	%No PIDs are to be filtered
	{struct, _Pids} = ej:get({<<"network-map">>},NetworkMap),
	{struct, utils:apply_attribute_filter_to_list(_Pids, AddressTypeFilter)};
filter_pids(undefined, NetworkMap, AddressTypeFilter) ->
	%If the attribute is not present we merely treat it as an empty array
	%(see case above).
	filter_pids([], NetworkMap, AddressTypeFilter);
filter_pids(PIDFilter, NetworkMap, AddressTypeFilter) ->
	{struct, filter_pids(PIDFilter, NetworkMap, AddressTypeFilter, [])}.
	
filter_pids([], _, _, AccIn) ->
	AccIn;
filter_pids([H|T], NetworkMap, AddressTypeFilter, AccIn) ->
	case ej:get({<<"network-map">>,H},NetworkMap) of
		undefined -> %When a PID is missing we keep processing
			filter_pids(T, NetworkMap, AddressTypeFilter, AccIn);
		Value ->
			NewValue = utils:apply_attribute_filter(AddressTypeFilter, Value),
			filter_pids(T, NetworkMap, AddressTypeFilter, [NewValue] ++ AccIn)
	end. 

%%%%%%%%%%%%%%%%%%%%%
%% Registration
%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Adds Mappings of the Base Path and the auto-generated
%% path to the ResourceId (typically a Cost-Map).
%% 
register_mapping(ResourceId, BasePath, CostMode, CostMetric) ->
	add_uri_mapping(ResourceId, ResourceId),
	add_uri_mapping(generate_path(BasePath, CostMode, CostMetric),ResourceId),
	ResourceId. 

register_mapping(BasePath, CostMode, CostMetric) ->
	_GeneratedPath = generate_path(BasePath, CostMode, CostMetric),
	add_uri_mapping(_GeneratedPath, _GeneratedPath),
	_GeneratedPath.

add_uri_mapping(K,V) ->
	try
		ets:insert(?URIMAPTBL, {K,V})
	catch error:badarg -> 
		urimap_table_init(),
		ets:insert(?URIMAPTBL, {K,V})
	end.
		
deregister_mapping(ResourcePath) ->
	deregister(get_id_for_path(ResourcePath)).

deregister_mapping(BasePath, CostMode, CostMetric) ->
	deregister_mapping(generate_path(BasePath, CostMode, CostMetric)).
	
deregister(K) ->
	try 
		URIs = ets:match(?URIMAPTBL, {'$1', K}),
		lists:foreach(fun(E) -> ets:delete(?URIMAPTBL,E) end, URIs)
	catch error:badarg ->
		urimap_table_init()
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
		ets:new(?URIMAPTBL, [named_table, set, public])
	catch
		error:badarg -> table_create_error
	end.

cost_filterservice_path_table() ->
		try
		ets:new(?COSTFILTERSVCTBL, [named_table, set, public])
	catch
		error:badarg -> table_create_error
	end.	
