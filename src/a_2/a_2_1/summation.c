// =====================================================================================
//       Filename:  summation.c
//    Description:  sum up 
//        Created:  26.10.2010 18:59:01
// =====================================================================================

#include <mpi.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

// typedefs
typedef int (*sendfunction)(void *buf, int count, MPI_Datatype datatype, int dest,
            int tag, MPI_Comm comm);

// function prototypes
void master (int size, int numpes, sendfunction send); // parameters: size of buffer, number of processing elements
void slave (int id, sendfunction send);


const int constant = 1;
char verbose = 0; // enable verbose output

int main (int argc, char **argv) {
	if (argc < 3) {
		fprintf (stderr, "Not enough arguments. \nUsage: summation_mpi <Number of elements in array> <Send Method> [verbose output, 0 = false, 1 = true]\n");
		fprintf (stderr, "where Send Method is d for MPI_SEND, s for MPI_SSEND, b for MPI_BSEND, r for MPI_RSEND\n");
		exit (EXIT_FAILURE);
	}


	// initialize MPI
	MPI_Init (&argc, &argv);

	int size = 1;

	assert (argc > 1);
	size = atoi (argv[1]);

	// determine the send function
	// TODO thread safe..? My local openmpi implementation creates one process for each PE.
	assert (argc > 2);
	sendfunction send = NULL;

	switch (*argv[2]) {
		case 'd':
			send = MPI_Send;
			break;
		case 's':
			send = MPI_Ssend;
			break;
		case 'b':
			send = MPI_Bsend;
			break;
		case 'r':
			send = MPI_Rsend;
			break;
		default:
			fprintf (stderr, "Option for sendmethod not recognized.Allowed values are d,s,b,r\n");
			exit (EXIT_FAILURE);
	}

	if (argc > 3) {
		verbose = atoi (argv[3]);
	}

	// we have to remember the number of PEs
	int numpes;
	MPI_Comm_size (MPI_COMM_WORLD, &numpes);

	// which rank does this process have?
	int myid;
	MPI_Comm_rank (MPI_COMM_WORLD, &myid);

	if (!myid) {
		/* master */
		// TODO fix bug - if size < numpes, it crashes
		master (size, numpes, send);
	} else {
		/* slave */
		slave (myid, send);
	}

	MPI_Finalize ();
	return EXIT_SUCCESS;
}

void master (int size, int numpes, sendfunction send) {

	char *arr = malloc (size * sizeof (char));
	MPI_Status stat;

	if (!arr) {
		fprintf (stderr, "Not enough memory to allocate %d bytes for the array.\n", size);
		exit (-2);
	}

	memset (arr, constant, size);

	// check the number of slaves
	if (numpes < 2) {
		// nothing to be done
		int sum = 0;
		int i;
		for (i = 0; i < size; i++) {
			sum += arr[i];
		}
		assert (sum == size * constant);
	} else {
		numpes = numpes - 1; //master is not adding array elements
		int slavenum = 1;
		int sum = 0;
		int i = 0;

		// partition the array according to the number of slaves
		int len = size / numpes; // the master doesn't help.
		int extra = size % numpes; //whatever is leftover

		for (i = 0; slavenum < numpes + 1; i += len) {
			if ( extra ) {
				//send len + 1 to get rid of leftovers
				MPI_Send (&(arr[i]), len + 1, MPI_CHAR, slavenum++, 0, MPI_COMM_WORLD);
				extra--;
				i++;
			}
			else {
				//no extra elemments left to add
				MPI_Send (&(arr[i]), len, MPI_CHAR, slavenum++, 0, MPI_COMM_WORLD);
			}
		}

		// Now, everybody got it's part of the array. The master waits for all responses and
		// sums them up.
		for (i = 1; i < numpes + 1; i++) {
			int part = 0;
			MPI_Recv (&part, 1, MPI_INT, i, 0, MPI_COMM_WORLD, &stat);
			sum += part;
		}

		if (verbose) {
			printf ("Result: %d\n", sum);
			printf ("Should be: %d\n", size * constant);
		}
		assert (sum == size * constant);

		free (arr);
	}
}

void slave (int myid, sendfunction send) {
	// determine length of message
	MPI_Status stat;
	MPI_Probe (MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &stat);

	int msglen;
	MPI_Get_count (&stat, MPI_CHAR, &msglen);
	assert (msglen > 0);
	char *arr = malloc (msglen * sizeof(char));

	if (!arr) {
		fprintf (stderr, "Not enough memory to allocate %d bytes for the array.", msglen);
		exit (-2);
	}

	MPI_Recv (arr, msglen, MPI_CHAR, stat.MPI_SOURCE, stat.MPI_TAG, MPI_COMM_WORLD, &stat);

	int cnt; // number of received elements
	MPI_Get_count (&stat, MPI_CHAR, &cnt);
	if (verbose) {
		printf ("%d: I got %d elements\n", myid, cnt);
	}

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
	free (arr);
}
