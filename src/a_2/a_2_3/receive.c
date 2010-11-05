/*
 * =====================================================================================
 *
 *       Filename:  receive.c
 *
 *    Description:  validate random order receiving mpi messages
 *
 *        Version:  1.0
 *        Created:  05.11.2010 11:48:21
 *       Revision:  none
 *       Compiler:  gcc
 *
 * =====================================================================================
 */

#include	<stdlib.h>
#include	<stdio.h>
#include	<string.h>
#include	<assert.h>
#include	<mpi.h>

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  main
 *  Description:  
 * =====================================================================================
 */
    int
main ( int argc, char *argv[] )
{
    // initialize MPI
    MPI_Init (&argc, &argv);

    // we have to remember the number of PEs
    int numpes;
    MPI_Comm_size (MPI_COMM_WORLD, &numpes);
    
    //for this we need 3 PEs
    assert(numpes == 3);

    // which rank does this process have?
    int myid;
    MPI_Comm_rank (MPI_COMM_WORLD, &myid);

    switch ( myid ) {
        case 0 :
            {
                printf("0: I have A....sending it!\n");
                char *msg = "A";
                MPI_Send(msg, strlen(msg) + 1, MPI_CHAR, 1, 0, MPI_COMM_WORLD);
                MPI_Send(msg, strlen(msg) + 1, MPI_CHAR, 2, 0, MPI_COMM_WORLD);
                break;
            }

        case 1 :
            {
                char *msg = "B";

                MPI_Status status;
                MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
                int msglen;
                MPI_Get_count(&status, MPI_CHAR, &msglen);
                assert(msglen > 0);
                char *recMsg = malloc(msglen * sizeof(char));
                MPI_Recv (recMsg, msglen, MPI_CHAR, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, &status);

                printf("1: I have B....\n");
                printf("1: Received an %s so sending my B\n", recMsg);
                MPI_Send(msg, strlen(msg) + 1, MPI_CHAR, 2, 0, MPI_COMM_WORLD);
                break;
            }

        case 2 :
            {
                int hasMail;
                int receivedNotAll = 2;
                MPI_Status status;

                while(receivedNotAll){

                    MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &hasMail, &status);

                    if (hasMail) {
                        int msglen;
                        MPI_Get_count(&status, MPI_CHAR, &msglen);
                        assert(msglen > 0);
                        char *msg = malloc(msglen * sizeof(char));

                        if (!msg) {
                            fprintf(stderr, "Could not allocate memory for %d bytes in message\n", msglen);
                            exit(-2);
                        }

                        MPI_Recv (msg, msglen, MPI_CHAR, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, &status);

                        printf("2: Received a %s\n", msg);
                        
                        free(msg);
                        receivedNotAll--;
                    }
                }
                break;
            }

        default:	
            break;
    }				/* -----  end switch  ----- */
    MPI_Finalize();
    return EXIT_SUCCESS;
}				/* ----------  end of function main  ---------- */
