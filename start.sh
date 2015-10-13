#!/bin/sh

export ERL_CRASH_DUMP_SECONDS=1

erl \
	-name ej_merge@`hostname` \
	-config app.config \
	-pa ebin deps/*/ebin .eunit \
	-boot start_sasl \
	-eval 'lists:foreach(fun(App)-> ok = application:start(App) end, [e_alto])'
