CC=mpicc
CLFAGS=-Wall

all: receive_mpi deadlock nodeadlock1 nodeadlock2

receive_mpi: receive.o
deadlock: deadlock.o
nodeadlock1: nodeadlock1.o
nodeadlock2: nodeadlock2.o

clean:
	-rm *.o
	-rm receive_mpi
