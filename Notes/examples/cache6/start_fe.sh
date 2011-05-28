#!/bin/bash

erl -boot start_sasl -pa cache_fe/ebin -name cache_fe@127.0.0.1 -s cache_fe_app