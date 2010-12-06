# use -DNDEBUG if you want to avoid some debugging symbols and function calls
CFLAGS= -DNDEBUG -Wall -pedantic 

CC=mpic++
wordcount_mpi: wordcount.cpp
	$(CC) $< -o $@ $(CFLAGS)

clean:
	@echo cleaning..
	-rm wordcount_mpi > /dev/null
	-rm reduced-* > /dev/null

.PHONY: clean
