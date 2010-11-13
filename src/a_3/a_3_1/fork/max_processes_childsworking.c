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

#include "messuretime.h"

static double delta = 0;
static const char *outputfile = "../data/fork_childsworking.dat";

static struct timeval begin, end;

static unsigned int output = 1;

void bufferEcho (void);

int main (int argc, char **argv){
	unsigned int numforks = 1000;

	if (argc > 1) {
		sscanf (argv[1], "%u", &numforks);
	}
	if (argc > 2) {
		sscanf (argv[2], "%u", &output);
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
			// let's do some calculating.
			/* 
			 The following code is compiled from the later loop (using gcc 4.5.1 for x86_64):
				400905:	83 7d f8 00          	cmpl   $0x0,-0x8(%rbp)						 # cid == 0?
				400909:	75 1f                	jne    40092a <main+0xf6>          # if cid != 0, skip.
				40090b:	c7 45 e8 01 00 00 00 	movl   $0x1,-0x18(%rbp)						 # i = 1
				400912:	90                   	nop
				400913:	8b 45 e8             	mov    -0x18(%rbp),%eax						 # %eax = i
				400916:	89 45 e8             	mov    %eax,-0x18(%rbp)            # i = %eax
				400919:	8b 45 e8             	mov    -0x18(%rbp),%eax            # %eax = i
				40091c:	85 c0                	test   %eax,%eax                   # %eax == %eax ?
				40091e:	75 f3                	jne    400913 <main+0xdf>          # if %eax != %eax, skip
				400920:	bf 00 00 00 00       	mov    $0x0,%edi                   # return code
				400925:	e8 86 fd ff ff       	callq  4006b0 <exit@plt>           # abort process
			 Therefore, -O2 (the default when calling gnu gcc) didn't optimize the loop, except for avoiding multiplication.
			*/
			volatile int i = 1;
			while (i *= 1); 
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
	printf("%d processes started %.8f ms/Process\n",i, delta/i); 
	
	if (output) {
		/* print data to file */
		FILE  *fp = fopen (outputfile, "a+");
		if (!fp) {
			fprintf (stderr, "Couldn't write to file ../data/fork.dat. Exiting disgracefully...\n");
			exit (EXIT_FAILURE);
		}

		// structure: processes time/fork delta(sum)
		fprintf (fp, "%d %.8f %.8f\n", i, delta / i, delta);
		fclose (fp);
	}
	exit(EXIT_SUCCESS);
}
