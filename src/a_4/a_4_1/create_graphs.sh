#!/bin/bash
#
# some m4 calls
#
#
m4 -DTITLE="Sum up array" -DFILENAME1="summation_first" \
	-DFILENAME2="summation_snd" -DFILE="summation" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array" -DFILENAME1="ascending_first" \
	-DFILENAME2="ascending_snd" -DFILE="ascending" gnuplot.gpi | gnuplot
