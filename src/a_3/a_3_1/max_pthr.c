/*
 * =====================================================================================
 *       Filename:  max_pthr.c
 *    Description:  Produce Posix-Threads until Death
 *        Created:  09.11.2010 20:43:23
 * =====================================================================================
 */
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <sys/time.h> 

clock_t begin, end;
int max_thr = 1000;

void* go(void* i){
	pthread_t th;
	long threadnr = (long)i;
	if(threadnr < max_thr){
		threadnr++;
		pthread_create(&th,NULL,go, (void *)threadnr);
		printf("Thread %d stareted!\n",threadnr);
		pthread_join(th,NULL);
	}else{
		end=clock();
		printf("CLOCKS_PER_SEC: %d\n",CLOCKS_PER_SEC);
		printf("%d Threads started %.8f Sec/Thread\n",max_thr,(float)(end-begin)/CLOCKS_PER_SEC);
	}
}


int main(){
	long i=1;
	pthread_t thr;
	printf("Starting first Thread...");
	begin=clock();
	if(pthread_create(&thr,NULL,go,(void *)i) < 0)
	perror("pthread_create failed: ");
	pthread_join(thr,NULL);
	exit(0);

}
