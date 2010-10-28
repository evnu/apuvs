CC=mpicc
CC=gcc -pthread -L/usr/lib/openmpi -lmpi -ldl -Wl,--export-dynamic -lnsl -lutil -lm -ldl
summation_mpi: summation.c
	$(CC) $< -o $@ -Wall	
