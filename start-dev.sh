#!/bin/sh
cd `dirname $0`
make
mkdir -p log/sasl
erl -sname bigwig -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config sys.config -s reloader -s bigwig
