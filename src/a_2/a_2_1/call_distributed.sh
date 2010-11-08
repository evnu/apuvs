#!/bin/bash
# Fajerski, Müller, Warnke - G02

# call summation on a number of hosts supplied by a machinefile

# globals
DATADIR=data/
OUTPUTFILE=risingnpcount_fixedsizearray
NUMBEROFPROCS=num_processors

USEMACHINEFILE=0

create_test_data () {
	# usage: 
	# $1 maximum number of processors
	# $2 size of array
	# $3 message send method
	# $4 outputfile in $DATADIR
	# $5 use machinefile? 1 == True, 0 == False [default]

	# maximum number of processors
	NUM=$1
	ARRAYSIZE=$2
	MODE=$3
	FINALOUTPUT=$4
	USEMACHINEFILE=$5

	MACHINEFILE=""

	case "$USEMACHINEFILE" in
		0) MACHINEFILE="" ;;
		1) MACHINEFILE="-machinefile machinefile" ;;
		*) echo "Parameter 5 not recognized, allowed values: 0 and 1"; exit 1;;
	esac

	# first, clean up old files
	echo removing old files
	rm $DATADIR/$OUTPUTFILE $DATADIR/$OUTPUTFILE $DATADIR/$NUMBEROFPROCS $DATADIR/$FINALOUTPUT 2&> /dev/null
	echo processing....

	for i in $(seq 1 $NUM)
	do
		echo $i PEs
		/usr/bin/time --format %e -o $DATADIR/$OUTPUTFILE -a \
			mpirun -np $i $MACHINEFILE ./summation_mpi $ARRAYSIZE $MODE
	done

	echo .... processing finished

	# paste the $DATADIR file and the number of $NUMBEROFPROCS
	seq 1 $NUM >> $DATADIR/$NUMBEROFPROCS

	echo "merging files"
	paste $DATADIR/$NUMBEROFPROCS $DATADIR/$OUTPUTFILE > $DATADIR/$FINALOUTPUT

	echo "done. result:"

	cat $DATADIR/$FINALOUTPUT

	echo "cleaning remaining files"
	rm $DATADIR/$OUTPUTFILE $DATADIR/$OUTPUTFILE $DATADIR/$NUMBEROFPROCS  2&> /dev/null
}

###############
# 1. Phase
# We sum up a fixed size array with a varying number of processors, both on a single
# machine and distributed over the machines defined in the "machinefile" using all
# possible send types
# 1.1 Array size: 1000000

# MPI_SEND
	# single machine
create_test_data 8 10000000 d fixedarray_1000000_defaultsendtype_singlemachine 0
	# distributed
create_test_data 8 10000000 d fixedarray_1000000_defaultsendtype_distributed 1

# MPI_SSEND
	# single machine
create_test_data 8 10000000 s fixedarray_1000000_synchronsendtype_singlemachine 0
	# distributed
create_test_data 8 10000000 s fixedarray_1000000_synchronsendtype_distributed 1

# MPI_BSEND
	# single machine
create_test_data 8 10000000 b fixedarray_1000000_blockingsendtype_singlemachine 0
	# distributed
create_test_data 8 10000000 b fixedarray_1000000_blockingsendtype_distributed 1

# MPI_RSEND
	# single machine
create_test_data 8 10000000 r fixedarray_1000000_readysendtype_singlemachine 0
	# distributed
create_test_data 8 10000000 r fixedarray_1000000_readysendtype_distributed 1
