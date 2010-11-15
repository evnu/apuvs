#!/bin/bash
#
# some m4 calls
#
#
m4 -DTITLE="Fork" -DFILENAME="fork" -DINPUT="data/fork.dat" gnuplot.gpi | gnuplot
m4 -DTITLE="Fork and Exec" -DFILENAME="fork_exec" -DINPUT="data/fork_exec.dat" gnuplot.gpi |gnuplot
m4 -DTITLE="Fork with working childs" -DFILENAME="fork_childsworking" -DINPUT="data/fork_childsworking.dat" gnuplot.gpi |gnuplot
m4 -DTITLE="pthread with sleeping threads" -DFILENAME="pthread" -DINPUT="data/pthread.dat" gnuplot.gpi |gnuplot
m4 -DTITLE="max_pthr" -DFILENAME="max_pthr" -DINPUT="data/max_pthr.dat" gnuplot.gpi | gnuplot
m4 -DTITLE="Java Threads" -DFILENAME="javathreads" -DINPUT="data/java.dat" gnuplot.gpi | gnuplot

