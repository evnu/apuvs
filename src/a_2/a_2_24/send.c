// =====================================================================================
//       Filename:  summation.c
//    Description:  sum up (see README)
//        Created:  26.10.2010 18:59:01
// =====================================================================================

#include <mpi.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

const int constant = 1;
/* finally a comment */
int main (int argc, char **argv) {

	int numpes; // number of processing units
	int myid;

	// initialize MPI
	MPI_Init (&argc, &argv);

	// we have to remember the number of PEs
	MPI_Comm_size (MPI_COMM_WORLD, &numpes);

	// which rank does this process have?
	MPI_Comm_rank (MPI_COMM_WORLD, &myid);

	if (!myid) {
		/* master */
        printf("If programm doesn't return MPI_Send is synchronous send\n");

        MPI_Send ("a", 2, MPI_CHAR, myid + 1, 0, MPI_COMM_WORLD);

        printf("MPI_Send returned....seems to be buffered\n");
	} else {
		/* slave */
        /* nothing to do */
	}

	MPI_Finalize ();
	return EXIT_SUCCESS;
}
