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
#include "messuretime.h"

static double delta = 0;
static struct timeval begin, end;
int max_thr = 100;

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
	gettimeofday(&begin, NULL);
	pthread_create(&thr, NULL, go, NULL);
	gettimeofday(&end, NULL);
	delta+=mdiff(&begin, &end);
	}
	pthread_join(thr,NULL);
	printf("Delta: %.8f\n ",delta);
	printf("Sec/Thread: %.8f\n",delta/max_thr);
	
	exit(0);

}
