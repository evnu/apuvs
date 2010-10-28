#!/bin/sh
#
# call summation with different arguments
#

# abort on Ctrl+C
trap 'echo Abort; exit 1' 2

for i in $(seq 1 8)
do
	# number of processors
	NUM=10000
	echo -n Using $i PEs to sum up $NUM elements
	time mpirun -np $i ./summation_mpi $NUM 0
	echo ""
done
