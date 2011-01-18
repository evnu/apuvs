#!/bin/sh
#
# check all erlang files in the current directory using dialyzer
#

# first, recompile
for i in *.erl
do
    erlc +debug_info $i
done

# now, check
for i in *.beam
do
    echo "Checking $i.."
    dialyzer $i
    echo ".. Done"
done
