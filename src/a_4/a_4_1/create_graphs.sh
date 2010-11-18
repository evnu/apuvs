#!/bin/bash
#
# some m4 calls
#
#
m4 -DTITLE="Sum up array" -DFILENAME1="summation_1st" \
	-DFILENAME2="summation_2nd" -DFILENAME3="summation_3rd" \
	-DFILE="summation" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array" -DFILENAME1="ascending_1st" \
	-DFILENAME2="ascending_2nd" -DFILENAME3="ascending_3rd" \
	-DFILE="ascending" gnuplot.gpi | gnuplot
