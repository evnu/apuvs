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

#include "messuretime.h"

struct timeval begin, end;
double delta;
int max_thr = 10000;

// mutex to avoid race conditions
// the time needed to acquire the mutex isn't messured
pthread_mutex_t taketime = PTHREAD_MUTEX_INITIALIZER;

// the thread function
void* go(void* i);

int main (int argc, char **argv){
	if (argc > 1) {
		sscanf (argv[1], "%d", &max_thr);
	}
	long i = 1;
	// just call the recursive function
	go ((void *) (&i));

	exit(EXIT_SUCCESS);
}

void *go (void *i) {
	long threadnr = * ((long*)(i));
	if(threadnr < max_thr){
		pthread_t th;
		pthread_mutex_lock (&taketime);
			threadnr++;
			gettimeofday (&begin, NULL);
				pthread_create(&th,NULL,go, (void *)(&threadnr));
			gettimeofday (&end, NULL);
			delta+=mdiff (&begin, &end);
		pthread_mutex_unlock (&taketime);
		pthread_join(th,NULL);
	}else{
		printf("CLOCKS_PER_SEC: %ld\n",CLOCKS_PER_SEC);
		printf("Delta: %ld\n",(long)delta);
		printf("%d Threads started %.8f Sec/Thread\n",max_thr,((float)delta/max_thr)/CLOCKS_PER_SEC);
	}
	return NULL; /* we have to return something.. NULL is better than some random value */
}
