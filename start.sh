#!/bin/sh

export ERL_CRASH_DUMP_SECONDS=1

erl \
	-name ej_merge@`hostname` \
	-config app.config \
	-pa ebin _build/default/lib/*/ebin .eunit \
	-boot start_sasl \
	-eval 'lists:foreach(fun(App)-> ok = application:start(App) end, [])'
