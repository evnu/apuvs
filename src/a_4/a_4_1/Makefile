.PHONY: clean

CFLAGS+=-std=c99 -fopenmp -Wall

all: summation summation_ascending_nums

summation: basic_functions.o messuretime.o
summation_ascending_nums: basic_functions.o messuretime.o

clean:
	-rm core summation summation_ascending_nums *.o data/*
