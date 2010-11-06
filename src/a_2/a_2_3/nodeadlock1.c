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

	// Avoiding deadlock by using MPI_Isend
	if (myid == 0) {
		// send message to 1, wait for message from 1
		char outputbuf[10000];
		char inputbuf[10000];
		MPI_Status stat;
		MPI_Request req;

		MPI_Isend (outputbuf, 10000, MPI_CHAR, 1, 0, MPI_COMM_WORLD, &req);
		MPI_Recv (inputbuf, 10000, MPI_CHAR, 1, 0, MPI_COMM_WORLD, &stat);
	} else {
		// send message to 1, wait for message from 1
		char outputbuf[10000];
		char inputbuf[10000];
		MPI_Status stat;
		MPI_Request req;

		MPI_Isend (outputbuf, 10000, MPI_CHAR, 0, 0, MPI_COMM_WORLD, &req);
		MPI_Recv (inputbuf, 10000, MPI_CHAR, 0, 0, MPI_COMM_WORLD, &stat);
	}
	printf ("done\n");
	MPI_Finalize ();

	return EXIT_SUCCESS;
}
