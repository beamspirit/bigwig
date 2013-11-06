#!/bin/sh
cd `dirname $0`
make
mkdir -p log/sasl
erl -name bigwig@localhost -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config sys.config -s reloader -s bigwig
