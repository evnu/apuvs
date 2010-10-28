#!/bin/sh
#
# call summation with different arguments
#

for i in $(seq 1 100)
do
	# number of processors
	time mpi_run -np $i ./summation_mpi 100000
done
