#!/bin/sh
# Fajerski, Müller, Warnke - G02
#

####
# abort on Ctrl+C
####
trap 'echo Abort; exit 1' 2

mpirun -np 2 ./send_block
