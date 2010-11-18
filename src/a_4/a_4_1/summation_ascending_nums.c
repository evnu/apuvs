// =====================================================================================
//       Filename:  summation.c
//    Description:  A summation again..
//        Created:  18.11.2010 12:57:58
// =====================================================================================

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "basic_functions.h"

#define ARBITRARY_BOUND 10000

int main (int argc, char **argv) {
	int array_size = 10;
	int sum = 0;

	struct timeval begin, end;

	if (argc > 1) {
		sscanf (argv[1], "%d", &array_size);
		if (array_size > ARBITRARY_BOUND) {
			fprintf (stderr, "Sorry. We only allow array_size < %d. Aborting.\n", ARBITRARY_BOUND);
			exit (EXIT_FAILURE);
		}
	}
	int *array = malloc (sizeof (int) * array_size);

	if (!array) {
		fprintf (stderr, "Couldn't allocate %d bytes of memory. Aborting.\n", array_size);
		exit (EXIT_FAILURE);
	}

	for (int i = 0; i < array_size; i++) {
		array[i] = i;
	}

	/* output protocol */
	protocol ();

	/* messure time */
	gettimeofday (&begin, NULL);

#pragma omp parallel shared(sum)
	{ /* Parallelized section */

		int internal_sum = 0;
#pragma omp for
		for (int i = 0; i < array_size; i++) {
			internal_sum += array[i];
		}

#pragma omp atomic
			sum += internal_sum;	
	}

	gettimeofday (&end, NULL);
	assert (sum == (array_size*(array_size+1))/2 - array_size);

	return EXIT_SUCCESS;
}

