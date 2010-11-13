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

clock_t begin, end, delta;
int max_thr = 10000;
int countsome = 0;

void* go(){
	return NULL; /* we have to return something.. NULL is better than some random value */
}


int main (int argc, char **argv){
	int i;
	if (argc > 1) {
		sscanf (argv[1], "%d", &max_thr);
	}
	
	pthread_t thr;
	
	for(i = 1;i<=max_thr;i++){
		begin=clock(); // TODO i don't really trust clock... maybe we should use gettimeofday?
			pthread_create(&thr,NULL,go,NULL);
		end=clock();
		delta+=end-begin;
		countsome = 0;
	}
	pthread_join(thr,NULL); // join the last thread
	printf("Delta: %d\n ",(int)delta);
	printf("Sec/Thread: %.8f\n",(double)(delta/max_thr)/(double)CLOCKS_PER_SEC);

	exit(0);
}
