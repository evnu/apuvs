#!/bin/sh
#
# call summation with different arguments
#

####
# abort on Ctrl+C
####
trap 'echo Abort; exit 1' 2

call5Times () {
	for i in $(seq 1 5)
	do
		eval	$1
	done
}


####
# calculation
####

echo "============ PHASE 1 ==========="
for i in $(seq 1 8)
do
	# number of processors
	NUM=10000
	echo  Using $i PEs to sum up $NUM elements 5 times
	# env: call /usr/bin/time instead of shell built-in
	# we try to reproduce each call 5 times to get some useable stats.
	time for j in $(seq 1 5)
	do
		mpirun -np $i ./summation_mpi $NUM 0
	done
	echo ""
done

echo "============ PHASE 2 ==========="
for i in 1000 10000 10000 100000 1000000 
do
	echo  Using 1 PEs to sum up $i elements 5 times
	time for j in $(seq 1 5)
	do
		mpirun -np 1 ./summation_mpi $i 0
	done
	echo ""
done

echo "============ PHASE 3 ==========="
for i in 1000 10000 10000 100000 1000000 
do
	echo  Using 4 PEs to sum up $i elements 5 times
	time for j in $(seq 1 5)
	do
		mpirun -np 4 ./summation_mpi $i 0
	done
	echo ""
done
