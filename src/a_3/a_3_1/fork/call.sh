#!/bin/bash

echo ............. first phase
for i in $(seq 1 500 30000)
do
	./max_processes $i
	pkill max_
	./max_processes_exec $i
	pkill max_
done
echo ............. max_processes_childsworking
for i in $(seq 1 10 500)
do
	./max_processes_childsworking $i
	pkill max_
done
