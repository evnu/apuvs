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
#include <alloca.h>

const int constant = 1;

int main (int argc, char **argv) {
	if (argc < 2) {
		fprintf (stderr, "Not enough arguments. \nUsage: summation_mpi <Number of elements in array> [verbose output, 0 = false, 1 = true]");
		exit (-1);
	}
	
	int size = 1;
	if (argc > 1) {
		size = atoi (argv[1]);
	}

	char verbose = 0;
	if (argc > 2) {
		verbose = atoi (argv[2]);
	}

	char *arr = alloca (size * sizeof (char));

	if (!arr) {
		fprintf (stderr, "Not enough memory to allocate %d bytes for the array.", size);
		exit (-2);
	}

	memset (arr, constant, size);

	int numpes; // number of processing units
	int myid;

	MPI_Status stat;

	// initialize MPI
	MPI_Init (&argc, &argv);
	
	// we have to remember the number of PEs
	MPI_Comm_size (MPI_COMM_WORLD, &numpes);

	// which rank does this process have?
	MPI_Comm_rank (MPI_COMM_WORLD, &myid);

	if (!myid) {
		/* master */

		// check the number of slaves
		if (numpes < 2) {
			// nothing to be done
			fprintf (stderr, "Using only one processor isn't very useful."); // TODO
		} else {
			int len = size / (numpes - 1); // the master doesn't help.
			int slavenum = 1;
			int i;
			int sum = 0;

			// partition the array according to the number of slaves
			for (i = 0; i < size && slavenum < numpes; i += len) {
				MPI_Send (&(arr[i]), len, MPI_CHAR, slavenum++, 0, MPI_COMM_WORLD);
			}

			// maybe we couldn't send all data before - add it now to the sum!
			// TODO maybe someone should check this O_o
			for (i = size / (numpes-1) * (numpes - 1); i < size; i++) {
				sum += arr[i];
			}

			// Now, everybody got it's part of the array. The master waits for all responses and
			// sums them up.
			for (i = 1; i < numpes; i++) {
				int part = 0;
				MPI_Recv (&part, 1, MPI_INT, i, 0, MPI_COMM_WORLD, &stat);
				sum += part;
			}

			if (verbose) {
				printf ("Result: %d\n", sum);
				printf ("Should be: %d\n", size * constant);
			}
			assert (sum == size * constant);
		}
	} else {
		/* slave */
		MPI_Recv (arr, size, MPI_CHAR, 0, 0, MPI_COMM_WORLD, &stat);

		int cnt; // number of received elements
		MPI_Get_count (&stat, MPI_CHAR, &cnt);
		if (verbose) {
			printf ("%d: I got %d elements\n", myid, cnt);
		}
		fflush (NULL);
		
		// sum
		int sum = 0;
		while (cnt) {
			sum += arr[--cnt];
		}
		if (verbose) {
			printf ("%d: sum = %d\n", myid, sum);
		}
		MPI_Send (&sum, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
		if (verbose) {
			printf ("%d: I send it.. \n", myid);
		}
	}

	MPI_Finalize ();
	return 0;
}

