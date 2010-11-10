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
float delta;

int main(){
	unsigned int numforks = 1000;

	begin = clock();
	// create 1000 processes
	pid_t pid = getpid ();
	pid_t cid;

	int i;

	for (i = 0; i < numforks; i++) {
		cid = fork ();
		if (cid == 0) {
			// wait for parent
			int stat;
			int options;

			waitpid (pid, &stat, 0);
			break;
		}
		if (cid < 0) {
			// error
			fprintf (stderr, "can't fork, error %d\n", errno);
			fprintf (stderr, "number of processes is %d\n", i);
			break;
		}
	}
	end = clock();

	if (cid > 0) {
		printf("CLOCKS_PER_SEC: %lu\n",CLOCKS_PER_SEC);
		printf("%d processes started %.8f Sec/Thread\n",i,
				(float)(end-begin)/CLOCKS_PER_SEC);
	}
	exit(EXIT_SUCCESS);
}
