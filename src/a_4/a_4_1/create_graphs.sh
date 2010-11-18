#!/bin/bash
#
# some m4 calls
#
#
m4 -DTITLE="Sum up array, averaging 10 runs" -DFILENAME1="summation_1st" \
	-DFILENAME2="summation_2nd" -DFILENAME3="summation_3rd" \
	-DFILE="summation" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array using median, median out of 10 runs" -DFILENAME1="summation_median_1st" \
	-DFILENAME2="summation_median_2nd" -DFILENAME3="summation_median_3rd" \
	-DFILE="summation.median" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array, fastest out of 10 runs" -DFILENAME1="summation_smallest_1st" \
	-DFILENAME2="summation_smallest_2nd" -DFILENAME3="summation_smallest_3rd" \
	-DFILE="summation.smallest" gnuplot.gpi | gnuplot

m4 -DTITLE="Sum up array, averaging 10 runs" -DFILENAME1="ascending_1st" \
	-DFILENAME2="ascending_2nd" -DFILENAME3="ascending_3rd" \
	-DFILE="ascending" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array using median, median out of 10 runs" -DFILENAME1="ascending_median_1st" \
	-DFILENAME2="ascending_median_2nd" -DFILENAME3="ascending_median_3rd" \
	-DFILE="ascending.median" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array, fastest out of 10 runs" -DFILENAME1="ascending_smallest_1st" \
	-DFILENAME2="ascending_smallest_2nd" -DFILENAME3="ascending_smallest_3rd" \
	-DFILE="ascending.smallest" gnuplot.gpi | gnuplot
