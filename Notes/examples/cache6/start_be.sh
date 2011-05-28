#!/bin/bash

erl -boot start_sasl -pa cache_be/ebin -name cache_be$1@127.0.0.1 -s cache_be_app