#include <sys/time.h>
double mdiff (struct timeval *start, struct timeval *end) {

	long seconds  = end->tv_sec  - start->tv_sec;
	double useconds = end->tv_usec - start->tv_usec;

	return ((seconds) * 1000 + useconds/1000.0) + 0.5;
}
