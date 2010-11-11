// create processes and let childs work
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
char *outputfile = "../data/fork_childsworking.dat";

void bufferEcho (void);

int main (int argc, char **argv){
	unsigned int numforks = 1000;

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
			// let's do some calculating.
			/* 
			 The following code is compiled from the later loop (using gcc 4.5.1 for x86_64):
				400913:	8b 45 e8             	mov    -0x18(%rbp),%eax				  # move i to %eax
				400916:	83 c0 01             	add    $0x1,%eax                # add 1 to %eax
				400919:	89 45 e8             	mov    %eax,-0x18(%rbp)         # move %eax to i
				40091c:	8b 45 e8             	mov    -0x18(%rbp),%eax         # move i to %eax
				40091f:	3d 9f 86 01 00       	cmp    $0x1869f,%eax            # compare eax with 10000
				400924:	7e ed                	jle    400913 <main+0xdf>				# this seems to be the right loop
			 Therefore, -O2 (the default when calling gnu gcc) didn't optimize the loop.
			*/
			volatile int i = 0;
			while (++i < 100000);
			exit (EXIT_SUCCESS);
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
	FILE  *fp = fopen (outputfile, "a+");
	if (!fp) {
		fprintf (stderr, "Couldn't write to file ../data/fork.dat. Exiting disgracefully...\n");
		exit (EXIT_FAILURE);
	}

	// structure: processes time/fork delta(sum)
	fprintf (fp, "%d %.8f %.8f\n", i, delta / i, delta);
	fclose (fp);
	exit(EXIT_SUCCESS);
}
