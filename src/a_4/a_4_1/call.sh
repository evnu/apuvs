#!/bin/bash
#
# call summation and summation_ascending_nums with differing parameters and output data to
# create graphs
#
#

##### global variables
DATADIR=data/

arraysize=$(seq 10 10 90; seq 100 100 900; seq 1000 1000 90000; seq 100000 100000 1000000)

for num_threads in $(seq 1 10)
do
	for size in $arraysize
		# $arraysize
	do 
		evaluation=$(
		for i in $(seq 1 10)
		do
			./summation $size $num_threads 
		done |grep TIME| awk '{sum=sum+$2;} END {printf sum/NR;}'
		)
		echo $num_threads $size $evaluation >> $DATADIR/"summation_$num_threads".dat
	done
done

