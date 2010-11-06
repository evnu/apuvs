CC=mpicc
CLFAGS=-Wall

all: receive_mpi deadlock nodeadlock1

receive_mpi: receive.o
deadlock: deadlock.o
nodeadlock1: nodeadlock1.o

clean:
	-rm *.o
	-rm receive_mpi
