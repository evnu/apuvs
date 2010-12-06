CC=mpic++
CFLAGS=-DNDEBUG -c -Wall -pedantic

all: wordcount_mpi

wordcount_mpi: mapreduce.o 
	$(CC) mapreduce.o wordcount.cpp -o wordcount_mpi


mapreduce.o: mapreduce.cpp
	$(CC) $(CFLAGS) mapreduce.cpp

wordcount.o: wordcount.cpp
	$(CC) $(CFLAGS) wordcount.cpp
clean:
	-rm wordcount_mpi *.o reduced-*
	
