#!/bin/bash

# due to the huge variance in execution times, we need to explore 
# usage when using 4 threads again.

##### global variables
DATADIR=data/


###### calculate median
median() {
  args=($( for i in $*; do echo $i; done | sort -n ))
  mid=$(( $#/2 ))
  if (( $#%2 == 0 )); then
    echo "scale=2;(${args[mid]}+${args[mid-1]})/2"|bc -l
  else
    echo ${args[mid]}
  fi
}

###### get smallest element
smallest () {
	args=($(for i in $*; do echo $i; done | sort -n))
	echo ${args[0]}
}

###### get maximum
biggest () {
	echo "The maximum function is _awefully_ broken!"
	args=($(for i in $*; do echo $i; done | sort -n | rev))
	echo ${args[0]}
}

###### calculate mean
average () {
	args=$(for i in $*; do echo $i; done | \
		awk '{sum=sum+$1;} END {printf sum/NR;}')
	echo $args
}



# summation.c
echo "Calling summation"
arraysize=$(seq 0 100 100000)

for size in $arraysize
do 
	evaluation=$(
	for i in $(seq 1 30)
	do
		./summation $size $num_threads 
	done |grep TIME | sed 's/TIME: //g'
	)
	echo $num_threads $size $(smallest $evaluation) >> $DATADIR/"4_summation.smallest".dat
	echo $num_threads $size $(smallest $evaluation) >> $DATADIR/"4_summation.biggest".dat
	echo $num_threads $size $(median $evaluation) >> $DATADIR/"4_summation.median".dat
	echo $num_threads $size $(average $evaluation) >> $DATADIR/"4_summation".dat
done

# summation_ascending_nums.c
echo "Calling summation_ascending_nums"

arraysize=$(seq 0 100 10000)

	for size in $arraysize
		# $arraysize
	do 
		evaluation=$(
		for i in $(seq 1 10)
		do
			./summation_ascending_nums $size $num_threads 
		done |grep TIME| sed 's/TIME: //g'
		)
		echo $num_threads $size $(smallest $evaluation) >> $DATADIR/"4_ascending.smallest.dat"
		echo $num_threads $size $(smallest $evaluation) >> $DATADIR/"4_ascending.biggest.dat"
		echo $num_threads $size $(median $evaluation) >> $DATADIR/"4_ascending.median".dat
		echo $num_threads $size $(average $evaluation) >> $DATADIR/"4_ascending".dat
	done
