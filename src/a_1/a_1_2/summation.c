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
		fprintf (stderr, "Not enough arguments. Usage: summation_mpi <Number of elements in array>");
		exit (-1);
	}

	int size = atoi (argv[1]);

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
		/* leader */
		//
		// check the number of slaves
		if (numpes < 2) {
			// nothing to be done
			fprintf (stderr, "Using only one processor isn't very useful."); // TODO
		} else {
			int len = size / (numpes - 1); // the master doesn't help.
			int slavenum = 1;
			int i;

			// partition the array according to the number of slaves
			for (i = 0; i < size && slavenum < numpes; i += len) {
				MPI_Send (&(arr[i]), len, MPI_CHAR, slavenum++, 0, MPI_COMM_WORLD);
			}

			int remaining = size - (numpes - 1) * len;
			int sum = 0;

			// maybe we couldn't send all data before - add it now to the sum!
			// TODO maybe someone should check this O_o
			for (i = size / (numpes-1) * (numpes - 1); i < size; i++) {
				sum += arr[i];
			}

			for (i = 1; i < numpes; i++) {
				int part = 0;
				MPI_Recv (&part, 1, MPI_INT, i, 0, MPI_COMM_WORLD, &stat);
				sum += part;
			}

			printf ("Result: %d\n", sum);
			printf ("Should be: %d\n", size * constant);
			assert (sum == size * constant);
		}
	} else {
		/* slave */
		MPI_Recv (arr, size, MPI_CHAR, 0, 0, MPI_COMM_WORLD, &stat);
		int cnt;
		MPI_Get_count (&stat, MPI_CHAR, &cnt);
		printf ("%d: Yes, i read it.. I got %d elements\n", myid, cnt);
		fflush (NULL);
		// build sum
		long long sum = 0;
		while (cnt) {
			sum += arr[--cnt];
		}
		printf ("%d: sum = %d\n", myid, sum);
		MPI_Send (&sum, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
		printf ("%d: I send it.. \n", myid);
	}

	MPI_Finalize ();
	return 0;
}

