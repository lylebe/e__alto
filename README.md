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

* - notes an exception to the standard

Install / Build
---------------
1. Install Erlang. The code is built using R18 but it does not use maps 
so it *may* work on earlier versions but is untested.
2. Install rebar3.
3. Clone
	<base path>$ git clone https://github.com/lylebe/e__alto.git
4. Copy rebar3 into the e__alto directory.
5. Build with rebar3 from the base directory. 
	e_alto$ ./rebar3 compile

Usage
-----
Quick Start - 
1. Run the start script.

	e_alto$ ./start.sh

2. Stopping server

	(e_alto@localhost) 1> e_alto_app:shutdown().
	
NOTE - Depending upon OS, erlang release, etc. you may need to halt the 
shell 

	(e_alto@localhost) 2> halt().

The IRD URI (by default) is http://localhost:8080/ 
and specified in app.config
