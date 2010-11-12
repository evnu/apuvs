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
int max_thr = 1000;

void* go(void* i){
	pthread_t th;
	long threadnr = (long)i;
	if(threadnr < max_thr){
		threadnr++;
		begin=clock();
		pthread_create(&th,NULL,go, (void *)threadnr);
		end=clock();
		delta+=(end-begin);
		pthread_join(th,NULL);
	}else{
		printf("CLOCKS_PER_SEC: %ld\n",CLOCKS_PER_SEC);
		printf("Delta: %ld\n",(long)delta);
		printf("%d Threads started %.8f Sec/Thread\n",max_thr,((float)delta/max_thr)/CLOCKS_PER_SEC);
	}

}


int main(){
	long i=1;
	pthread_t thr;
	begin=clock();
	if(pthread_create(&thr,NULL,go,(void *)i) >= 0){
		end=clock();
		delta=end-begin;
		pthread_join(thr,NULL);
	}
	else
		perror("pthread_create failed: ");
	exit(0);

}
