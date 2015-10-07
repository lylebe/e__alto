e_alto
======
This is a series of libraries and application that supports the 
IETF Application Layer Transport Optimization (ALTO) core specification 
(RFC 7285) and, as opportunities permit, related standards.

AT THIS TIME THE CODE IS ALPHA and undergoing various tests while 
final code functions are delivered for the next release.

Funtcionality that has been tested
- Map and Map Filter Services
- Costmap and Costmap Filter Services (with constraint support)
- Endpoint Property Service
- Endpoint Cost Service (including fine grain search constraint support*)

TODO
- Set up and pass the ALTO 93 interop tests.
- Automatically start the Server.

* - notes an exception to the standard

Install / Build
---------------
1. Install Erlang. The code is built using R16B03 but it does not use maps 
so it *may* work on earlier versions but is untested.
2. Build with rebar (install if you don't have it).  From the base 
directory.  (Don't forget to get dependencies first!)
> e_alto$ ./rebar get-deps
> e_alto$ ./rebar compile

Usage
----- 
1. Run the start script.

	e_alto$ ./start.sh

2. At the erlang console start the server (the script will be fixed 
later).
  
	e_alto:start().

The IRD URI (by default) is http://localhost:8080/ 
and specified in app.config

KNOWN ISSUES/LIMITATIONS
------------------------
1. Manual start from console. 
