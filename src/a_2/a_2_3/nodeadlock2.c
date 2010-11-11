// Fajerski, Müller, Warncke - G02
// =====================================================================================
//       Filename:  nodeadlock2.c
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

	// deadlock avoidance: PE 0 sends and recieves using the same function call, PE 1 uses
	// its own buffer to avoid blocking on send.
	if (myid == 0) {
		// send message to 1, wait for message from 1
		char buf[10000];
		MPI_Status stat;

		MPI_Sendrecv_replace (buf, 10000, MPI_CHAR, 1, 0, 1, 0,MPI_COMM_WORLD, &stat);
		printf ("0: done\n");
	} else {
		// send message to 0, wait for message from 0
		char buf[10000];
		char intermediate_buffer[10000 + MPI_BSEND_OVERHEAD];
		MPI_Buffer_attach (&intermediate_buffer, 10000 + MPI_BSEND_OVERHEAD);

		MPI_Status stat;
		MPI_Bsend (buf, 10000, MPI_CHAR, 0, 0, MPI_COMM_WORLD);
		// we can use buf again, as intermediate_buffer will take care of buffering
		MPI_Recv (buf, 10000, MPI_CHAR, 0, 0, MPI_COMM_WORLD, &stat);
		printf ("1: done\n");
	}
	MPI_Finalize ();

	return EXIT_SUCCESS;
}
