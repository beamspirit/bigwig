#!/bin/sh
cd `dirname $0`
make
exec erl -sname bigwig -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -config sys.config -s reloader -s bigwig
