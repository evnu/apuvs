/*
 * =====================================================================================
 *       Filename:  max_pthr.c
 *    Description:  Produce Posix-Threads until Death
 *        Created:  09.11.2010 20:43:23
 * =====================================================================================
 */
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h> 
#include <sys/wait.h> 
#include <time.h>
#include <unistd.h>

clock_t begin, end;
double delta = 0;

int main (int argc, char **argv){
	unsigned int numforks = 10000;

	if (argc > 1) {
		sscanf (argv[1], "%u", &numforks);
	}

	// create 1000 processes
	pid_t pid = getpid ();
	pid_t cid;

	int i;

	for (i = 0; i < numforks; i++) {
		begin = clock();
			cid = fork ();
		end = clock();
		// TODO clock wirft wohl zu große werte.. 1 s pro fork() ? im leben nicht..
		// master counts actual creation time
		
		if (cid > 0) {
			delta += end-begin;
		}
		if (cid == 0) {
			/* wait for parent */
			// We want the childs to live until we are done with creating processes, but they
			// shouldn't waste any processing time (actual processing isn't required according
			// to the excercise description)
			int stat;

			waitpid (pid, &stat, 0);
			exit (EXIT_SUCCESS); /* child doesn't need to clean up or print */
		}
		if (cid < 0) {
			/* error */
			// NOTE: error 11 == EAGAIN
			// see man errno for more informatioen
			fprintf (stderr, "can't fork anymore, error %d. See man errno.\n", errno);
			fprintf (stderr, "number of processes is %d\n", i);
			break;
		}
	}
	printf ("Delta is: %.8f\n", delta);
	// only parent process reaches this line
	printf("CLOCKS_PER_SEC: %lu\n",CLOCKS_PER_SEC);
	printf("%d processes started %.8f Sec/Thread\n",i,
			delta/CLOCKS_PER_SEC); // TODO stimmt nicht
	exit(EXIT_SUCCESS);
}
