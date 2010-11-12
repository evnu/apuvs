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
#include <sys/time.h> 

#include "messuretime.h"

// we only need compilation unit scope for this variables
static struct timeval begin, end;
static double delta;
static int max_thr = 10000;

// mutex to avoid race conditions
// the time needed to acquire the mutex isn't messured
pthread_mutex_t taketime = PTHREAD_MUTEX_INITIALIZER;

// the thread function
static void* go(void* i);

int main (int argc, char **argv){
	long i = 1;
	if (argc > 1) {
		(void) sscanf (argv[1], "%d", &max_thr);
	}

	// just call the recursive function
	(void) go ((void *) (&i)); // cast to avoid splint warning message

	exit(EXIT_SUCCESS);
}

void *go (void *i) {
	long threadnr = * ((long*)(i));
	int pthread_error_number;
	pthread_t th;

	if(threadnr < max_thr){
		if ((pthread_error_number = pthread_mutex_lock (&taketime))) {
			printf ("I couldn't acquire a lock in line %d due to error number %d\n",
					__LINE__, pthread_error_number);
			exit (EXIT_FAILURE);
		}
		threadnr++;
		gettimeofday (&begin, NULL);
		if ((pthread_error_number = pthread_create(&th, 
																	NULL /*@null@*/,
																	go, (void *)(&threadnr)
																	)
			 ))
		{
			printf ("Aborting due to errno = %d\n", pthread_error_number);
			printf("CLOCKS_PER_SEC: %ld\n",CLOCKS_PER_SEC);
			printf("Delta: %ld\n",(long)delta);
			printf("%d Threads started %.8f Sec/Thread\n",max_thr,((float)delta/max_thr)/CLOCKS_PER_SEC);
		}
		gettimeofday (&end, NULL);
		delta += mdiff (&begin, &end);
		(void) pthread_mutex_unlock (&taketime);
		(void) pthread_join(th,NULL);
	}else{
		printf("CLOCKS_PER_SEC: %ld\n",CLOCKS_PER_SEC);
		printf("Delta: %ld\n",(long)delta);
		printf("%d Threads started %.8f Sec/Thread\n",max_thr,((float)delta/max_thr)/CLOCKS_PER_SEC);
	}
	return NULL; /* we have to return something.. NULL is better than some random value */
}