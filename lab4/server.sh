#!/bin/dash
erl -pa ebin -pa deps/gen_nb_server/ebin -boot start_sasl -s cache_app
