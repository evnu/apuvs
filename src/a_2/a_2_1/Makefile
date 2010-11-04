CC=mpicc
summation_mpi: summation.c
	$(CC) $< -o $@ -Wall --std=c99 -pedantic

clean:
	-rm *.o
	-rm summation_mpi
