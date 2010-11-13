/*
 * =====================================================================================
 *       Filename:  max_pthr.c
 *    Description:  Produce Posix-Threads and measure the time to create them 
 *    		    while each thread is doing something
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

void* go(){
	volatile int i = 1;
	while (i *= 1); 
	return NULL; 
}


int main (int argc, char **argv){
	int i;
	if (argc > 1) {
		sscanf (argv[1], "%d", &max_thr);
	}
	
	pthread_t thr;
		
	for(i = 1;i<=max_thr;i++){
	begin=clock();
	pthread_create(&thr,NULL,go,NULL);
	end=clock();
	delta+=end-begin;
	}
	printf("Delta: %d\n ",(int)delta);
	printf("Sec/Thread: %.8f\n",((double)delta/(double)max_thr)/(double)CLOCKS_PER_SEC);

	exit(0);

}
