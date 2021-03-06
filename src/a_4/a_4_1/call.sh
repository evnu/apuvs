#!/bin/bash
#
# call summation and summation_ascending_nums with differing parameters and output data to
# create graphs
#
#

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
	args=($(for i in $*; do echo $i; done | sort -rn))
	echo ${args[0]}
}

###### calculate mean
average () {
	args=$(for i in $*; do echo $i; done | \
		awk '{sum=sum+$1;} END {printf sum/NR;}')
	echo $args
}

################ summation.c
echo "Calling summation"

arraysize=$(seq 0 500 100000)

for num_threads in $(seq 1 10)
do
	for size in $arraysize
		# $arraysize
	do 
		evaluation=$(
		for i in $(seq 1 30)
		do
			./summation $size $num_threads 
		done |grep TIME | sed 's/TIME: //g'
		)
		echo $num_threads $size $(smallest $evaluation) >> $DATADIR/"summation.smallest.$num_threads".dat
		echo $num_threads $size $(biggest $evaluation) >> $DATADIR/"summation.biggest.$num_threads".dat
		echo $num_threads $size $(median $evaluation) >> $DATADIR/"summation.median.$num_threads".dat
		echo $num_threads $size $(average $evaluation) >> $DATADIR/"summation.$num_threads".dat
	done
done

################ summation_ascending_nums.c

echo "Calling summation_ascending_nums"

arraysize=$(seq 0 500 10000)

for num_threads in $(seq 1 10)
do
	for size in $arraysize
		# $arraysize
	do 
		evaluation=$(
		for i in $(seq 1 10)
		do
			./summation_ascending_nums $size $num_threads 
		done |grep TIME| sed 's/TIME: //g'
		)
		echo $num_threads $size $(smallest $evaluation) >> $DATADIR/"ascending.smallest.$num_threads".dat
		echo $num_threads $size $(biggest $evaluation) >> $DATADIR/"ascending.biggest.$num_threads".dat
		echo $num_threads $size $(median $evaluation) >> $DATADIR/"ascending.median.$num_threads".dat
		echo $num_threads $size $(average $evaluation) >> $DATADIR/"ascending.$num_threads".dat
	done
done
