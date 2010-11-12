#!/bin/bash
#
# some m4 calls
#
#
m4 -DTITLE="Fork" -DFILENAME="fork" -DINPUT="data/fork.dat" gnuplot.gpi | gnuplot
m4 -DTITLE="Fork and Exec" -DFILENAME="fork_exec" -DINPUT="data/fork_exec.dat" gnuplot.gpi |gnuplot
m4 -DTITLE="Fork with working childs" -DFILENAME="fork_childsworking" -DINPUT="data/fork_childsworking.dat" gnuplot.gpi |gnuplot

