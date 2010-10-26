#include <mpi.h>
#include <stdio.h>
#include <string.h>

#define BUFSIZE 128
#define TAG 0

int main (int argc, char **argv) {
	char idstr[32];
	char buff[BUFSIZE];
	int numprocs;
	int myid;
	int i;
	MPI_Status stat;

	MPI_Init (&argc, &argv); // initialize
	MPI_Comm_size(MPI_COMM_WORLD,&numprocs); // how big is the world?
	MPI_Comm_rank(MPI_COMM_WORLD, &myid); // processor's rank

	if (myid == 0) {
		printf ("%d: We have %d processors\n", myid, numprocs);
		for (i = 1; i < numprocs; i++) {
			sprintf (buff, "Hello %d!\n", i);
			MPI_Send (buff, BUFSIZE, MPI_CHAR, i, TAG, MPI_COMM_WORLD);
		}
		for (i = 1; i < numprocs; i++) {
			MPI_Recv (buff, BUFSIZE, MPI_CHAR, i, TAG, MPI_COMM_WORLD, &stat);
			printf ("%d: %s\n", myid, buff);
		}
	} else {
		 // receive from rank 0
		 MPI_Recv (buff, BUFSIZE, MPI_CHAR, 0, TAG, MPI_COMM_WORLD, &stat);
		 sprintf (idstr, "Processor %d ", myid);
		 strcat (buff, idstr);
		 strcat (buff, " reporting for duty\n");
		 // send back to rank 0
		 MPI_Send (buff, BUFSIZE, MPI_CHAR, 0, TAG, MPI_COMM_WORLD);
	}

	MPI_Finalize();
	return 0;
}
