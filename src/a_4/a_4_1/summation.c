// =====================================================================================
//       Filename:  summation.c
//    Description:  A summation again..
//        Created:  18.11.2010 12:57:58
// =====================================================================================

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <omp.h>
#include <assert.h>

#include "basic_functions.h"

int main (int argc, char **argv) {
	int array_size = 100;
	int sum = 0;

	if (argc > 1) {
		sscanf (argv[1], "%d", &array_size);
	}
	char *array = malloc (array_size);
	
	if (!array) {
		fprintf (stderr, "Couldn't allocate %d bytes of memory. Aborting.\n", array_size);
		exit (EXIT_FAILURE);
	}

	memset (array, 1, array_size);

	/* output protocol */
	protocol ();

#pragma omp parallel shared(sum)
	{ /* Parallelized section */

		printf ("this is thread %d\n", omp_get_thread_num ());
		int internal_sum = 0;
#pragma omp for
		for (int i = 0; i < array_size; i++) {
			internal_sum += array[i];
		}

#pragma omp atomic
			sum += internal_sum;	
	}

	assert (sum == array_size);

	return EXIT_SUCCESS;
}

