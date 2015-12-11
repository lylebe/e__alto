#!/bin/sh

export ERL_CRASH_DUMP_SECONDS=1

erl \
	-name e_alto@`hostname --fqdn` \
	-config app.config \
	-pa ebin _build/default/lib/*/ebin .eunit \
	-boot start_sasl \
	-s e_alto_app
