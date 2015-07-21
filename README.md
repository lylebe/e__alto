e_alto
======

This is a series of libraries and eventually an application that will 
support the IETF Application Layer Transport Optimization (ALTO) core
specification (RFC 7285) and, as opportunities permit, related 
standards.

AT THIS TIME THE CODE IS PRE-ALPHA and UNTESTED but is being pushed out 
to get assistance from others.

-----
To use 
1. Run the start script.
e_alto$ ./start.sh

You will see several initializaton activites in the erlang console.

2. At the erlang console start the server (the script will be fixed 
later).
  
1> e_alto:start().

The server starts and listens on 8080.

Two URIs are available 
http://localhost:8080/ (Information Resource Directory)
http://localhost:8080/networkmap (Map Service - loads the default 
network map found in the app.config variable defaultmaploc

KNOWN ISSUES
1. Map loading - right now the maps are only checked for conformance to
JSON and semantic parsing.  JSON Schema is not applied so maps that are 
not conformant can be loaded.
2. Manual start from console. 
