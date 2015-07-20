-module(mapservices_tests).

-define(APPLICATIONNAME,e_alto).

-export([ map_test/0 ]).
	
map_test() ->
	e_alto:load_application(),
	lager:info("Application loaded",[]),
	{ok, _SchemaLoc} = application:get_env(?APPLICATIONNAME, schema),
	{ok, _DefMapName} = application:get_env(?APPLICATIONNAME, default-map),
	
	{ok, _DefMapLoc} = application:get_env(?APPLICATIONNAME, defaultmaploc),
	%Parse the file
	{ok, _File} = file:read_file(_DefMapLoc),
	mapservices:set_map(_File),
	ok.
		
	
