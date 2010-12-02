# use -DNDEBUG if you want to avoid some debugging symbols and function calls
CFLAGS=-Wall -pedantic 

CC=mpic++
wordcount_mpi: wordcount.cpp
	$(CC) $< -o $@ $(CFLAGS)

clean:
	@echo cleaning..
	-rm wordcount_mpi > /dev/null

.PHONY: clean
