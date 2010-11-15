/*
 * =====================================================================================
 *       Filename:  max_pthr.c
 *    Description:  Produce Posix-Threads and measure the time to create them
 *        Created:  09.11.2010 20:43:23
 * =====================================================================================
 */
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include "messuretime.h"

static double delta = 0;
static struct timeval begin, end;
int max_thr = 10000;
static const char *outputfile = "./data/max_pthr.dat";
static unsigned int output = 1;

static char done = 0;

void* go(){
	while (!done)
		sleep (100);
	return NULL; 
}

int main (int argc, char **argv){
	int i;
	if (argc > 1) {
		sscanf (argv[1], "%d", &max_thr);
	}
	if (argc > 2) {
		sscanf (argv[2], "%u", &output);
	}
	
	pthread_t thr;
	
	for(i = 1;i<=max_thr;i++){
		gettimeofday(&begin,NULL); 
		int i = pthread_create(&thr,NULL,go,NULL);
		gettimeofday(&end,NULL);
		if(i!=0){
			printf("pthread_create failed! Abort...");
			exit(-1);
		}
		delta+=mdiff(&begin,&end);
	}
	// set "done"
	done = 1;

	// wait for last thread to complete
	pthread_join(thr,NULL); 
	printf("Delta: %.8f\n ",delta);
	printf("%.8fms/Thread\n",delta/max_thr);

	if(output){
		FILE  *fp = fopen (outputfile, "a+");
		if (!fp) {
			fprintf (stderr, "Couldn't write to file ../data/max_pthr.dat. Exiting disgracefully...\n");
			exit (EXIT_FAILURE);
		}

		// structure: processes time/thread delta(sum)
		fprintf (fp, "%d %.8f %.8f\n", max_thr, delta / max_thr, delta);
		fclose (fp);

	}
	exit(0);
}
