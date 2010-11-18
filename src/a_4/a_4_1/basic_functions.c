#include <omp.h>
#include <stdio.h>

void protocol (void) {
	// only useful when called in parallel section
	// printf ("Number of threads: %d\n", omp_get_num_threads ());
	printf ("Maximum number of threads: %d\n", omp_get_max_threads ());
	printf ("Number of available processors: %d\n", omp_get_num_procs ());
}
