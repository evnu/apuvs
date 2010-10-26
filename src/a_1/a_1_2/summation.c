// =====================================================================================
//       Filename:  summation.c
//    Description:  sum up (see README)
//        Created:  26.10.2010 18:59:01
// =====================================================================================

#include <mpi.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#define SIZE 100

int main (int argc, char **argv) {
	int constant = 1;
	char arr[SIZE];
	memset (arr, constant, SIZE);

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
			fprintf (stderr, "Nah.. not really useful."); // TODO
		} else {
			int len = SIZE / (numpes - 1); // the master doesn't help.
			int slavenum = 1;
			int i;

			// partition the array according to the number of slaves
			for (i = 0; i < SIZE && slavenum < numpes; i += len) {
				MPI_Send (&(arr[i]), len, MPI_CHAR, slavenum++, 0, MPI_COMM_WORLD);
			}

			int remaining = SIZE - (numpes - 1) * len;
			int sum = 0;

			// maybe we couldn't send all data before - add it now to the sum!
			// TODO maybe someone should check this O_o
			for (i = SIZE / (numpes-1) * (numpes - 1); i < SIZE; i++) {
				sum += arr[i];
			}

			for (i = 1; i < numpes; i++) {
				int part = 0;
				MPI_Recv (&part, 1, MPI_INT, i, 0, MPI_COMM_WORLD, &stat);
				sum += part;
			}

			printf ("Result: %d\n", sum);
			printf ("Should be: %d\n", SIZE * constant);
		}
	} else {
		/* slave */
		MPI_Recv (arr, SIZE, MPI_CHAR, 0, 0, MPI_COMM_WORLD, &stat);
		int cnt;
		MPI_Get_count (&stat, MPI_CHAR, &cnt);
		printf ("Yes, i read it.. I got %d elements\n", cnt);
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

