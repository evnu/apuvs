// create processes, exec and messure time
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h> 
#include <sys/wait.h> 
#include <time.h>
#include <unistd.h>
#include <assert.h>

#include "messuretime.h"

double delta = 0;
char *outputfile = "../data/fork_exec.dat";

struct timeval begin, end;

void bufferEcho (void);

int main (int argc, char **argv){

	bufferEcho ();

	unsigned int numforks = 10;

	if (argc > 1) {
		sscanf (argv[1], "%u", &numforks);
	}

	// maybe it isn't sane to start more than 100000 processes
	assert (numforks < 100000);

	pid_t cid;

	int i;

	for (i = 0; i < numforks; i++) {
		// master counts actual creation time
		gettimeofday(&begin, NULL);
			cid = fork ();
		gettimeofday(&end, NULL);
		
		if (cid > 0) {
			delta += mdiff (&begin, &end);
		}
		if (cid == 0) {
			// we exec echo to see how exec affects cpu load
			if (execlp ("echo", "echo", "-n", "", NULL)) {
				/* if exec returns, something went badly wrong */
				fprintf (stderr, "ERROR: calling exec produced error %d\n", errno);
				exit (EXIT_FAILURE);
			}
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
	printf("%d processes started %.8f ms/Process\n",i,
			delta/i); 
	
	/* print data to file */
	FILE  *fp = fopen (outputfile, "a+");
	if (!fp) {
		fprintf (stderr, "Couldn't write to file %s. Exiting disgracefully...\n", outputfile);
		exit (EXIT_FAILURE);
	}

	// structure: processes time/fork delta(sum)
	fprintf (fp, "%d %.8f %.8f\n", i, delta / i, delta);
	fclose (fp);
	exit(EXIT_SUCCESS);
}

// to make sure that echo is cached, we'll execute it once. Of course that doesn't
// guarantee anything, but nevermind.

void bufferEcho (void) {
	pid_t id = fork ();
	// child is supposed to echo nothing
	if (!id) {
		if (execlp ("echo", "echo", "-n", "", NULL)) {
			/* if exec returns, something went badly wrong */
			fprintf (stderr, "ERROR: calling exec produced error %d\n", errno);
			exit (EXIT_FAILURE);
		}
	}
}
