// create processes and messure time
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

// accumulated time in ms
double delta = 0;
char *outputfile = "../data/fork.dat";

struct timeval begin, end;

int main (int argc, char **argv){
	unsigned int numforks = 10;

	if (argc > 1) {
		sscanf (argv[1], "%u", &numforks);
	}
	
	// maybe it isn't sane to start more than 100000 processes
	assert (numforks < 100000);

	pid_t pid = getpid ();
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
			/* wait for parent */

			// see Aufgabe 3.2. We could also use waitpid, but sleep does the tric as well.
			while (1)
				sleep (100);

			// Use the following code if the upper infinite loop is too ugly.
			// We want the childs to live until we are done with creating processes, but they
			// shouldn't waste any processing time (actual processing isn't required according
			// to the excercise description)
			// int stat;
			// waitpid (pid, &stat, 0);

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
	printf("%d processes started %.8f ms/Process\n", i, delta/i); 
	
	/* print data to file */
	FILE  *fp = fopen (outputfile, "a+");

	if (!fp) {
		fprintf (stderr, "Couldn't write to file %s. Exiting disgracefully...\n", outputfile);
		exit (EXIT_FAILURE);
	}
	// structure: processes time/fork
	fprintf (fp, "%d %.8f %.8f\n", i, delta / i, delta);
	fclose (fp);
	exit(EXIT_SUCCESS);
}
