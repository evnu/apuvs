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
#include "messuretime.h"

int main (int argc, char **argv) {
	int array_size = 100;
	int sum = 0;

	struct timeval begin, end;

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

	assert (sum == array_size);

	printf ("TIME: %.8f\n", mdiff (&begin, &end));

	return EXIT_SUCCESS;
}

