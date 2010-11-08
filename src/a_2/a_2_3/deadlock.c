// Fajerski, Müller, Warnke - G02
// =====================================================================================
//       Filename:  deadlock.c
//        Created:  06.11.2010 11:51:59
// =====================================================================================
#include <mpi.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char **argv) {
	// initialize MPI
	MPI_Init (&argc, &argv);

	// we have to remember the number of PEs
	int numpes;
	MPI_Comm_size (MPI_COMM_WORLD, &numpes);

	//for this we need 2 PEs
	assert(numpes == 2);

	// which rank does this process have?
	int myid;
	MPI_Comm_rank (MPI_COMM_WORLD, &myid);
	if (myid == 0) {
		// send message to 1, wait for message from 1
		char buf[10000]; 
		MPI_Status stat;

		MPI_Send (buf, 10000, MPI_CHAR, 1, 0, MPI_COMM_WORLD);
		MPI_Recv (buf, 10000, MPI_CHAR, 1, 0, MPI_COMM_WORLD, &stat);
		printf ("0: Received %c and %c\n", buf[0], buf[1]);
	} else {
		// send message to 1, wait for message from 1
		char buf[10000];
		MPI_Status stat;

		MPI_Send (buf, 10000, MPI_CHAR, 0, 0, MPI_COMM_WORLD);
		MPI_Recv (buf, 10000, MPI_CHAR, 0, 0, MPI_COMM_WORLD, &stat);
		printf ("1: Received %c and %c\n", buf[0], buf[1]);
	}
	MPI_Finalize ();

	return EXIT_SUCCESS;
}
