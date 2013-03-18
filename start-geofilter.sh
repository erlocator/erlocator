#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -name erlocator -setcookie erlocator -s reloader -s geofilter
