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

clock_t begin, end;
double delta = 0;

void bufferEcho (void);

int main (int argc, char **argv){

	bufferEcho ();

	unsigned int numforks = 1;

	if (argc > 1) {
		sscanf (argv[1], "%u", &numforks);
	}

	// maybe it isn't sane to start more than 100000 processes
	assert (numforks < 100000);

	pid_t cid;

	int i;

	for (i = 0; i < numforks; i++) {
		// master counts actual creation time
		begin = clock();
			cid = fork ();
		end = clock();
		
		if (cid > 0) {
			delta += (end-begin)/(double)CLOCKS_PER_SEC; // divide to convert to seconds
		}
		if (cid == 0) {
			// we exec ls to see how long exec takes approximately.
			if (execlp ("ls", "ls", NULL)) {
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
	printf("CLOCKS_PER_SEC: %lu\n",CLOCKS_PER_SEC);
	printf("%d processes started %.8f Sec/Process\n",i,
			delta/i); 
	
	/* print data to file */
	FILE  *fp = fopen ("../data/fork_exec.dat", "a+");
	if (!fp) {
		fprintf (stderr, "Couldn't write to file ../data/fork_exec.dat. Exiting disgracefully...\n");
		exit (EXIT_FAILURE);
	}

	// structure: processes time/fork
	fprintf (fp, "%d %.8f\n", i, delta / i);
	fclose (fp);
	exit(EXIT_SUCCESS);
}

// to make sure that echo is cached, we'll execute it once. Of course that doesn't
// guarantee anything, but nevermind.

void bufferEcho (void) {
	pid_t id = fork ();
	// child is supposed to ls
	if (!id) {
		if (execlp ("ls", "ls", NULL)) {
			/* if exec returns, something went badly wrong */
			fprintf (stderr, "ERROR: calling exec produced error %d\n", errno);
			exit (EXIT_FAILURE);
		}
	}
}
