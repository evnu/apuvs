#!/bin/sh
#

####
# abort on Ctrl+C
####
trap 'echo Abort; exit 1' 2

call5Times () {
	for i in $(seq 1 5)
	do
		mpirun -np 3 receive_mpi
        echo ""
	done
}


####
# calculation
####

echo "============ Calling receive 5 times ==========="
call5Times
