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

m4 -DTITLE="Sum up array with ascending numbers, averaging 10 runs" -DFILENAME1="ascending_1st" \
	-DFILENAME2="ascending_2nd" -DFILENAME3="ascending_3rd" \
	-DFILE="ascending" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array with ascending numbers using median out of 10 runs" -DFILENAME1="ascending_median_1st" \
	-DFILENAME2="ascending_median_2nd" -DFILENAME3="ascending_median_3rd" \
	-DFILE="ascending.median" gnuplot.gpi | gnuplot
m4 -DTITLE="Sum up array with ascending numbers, fastest out of 10 runs" -DFILENAME1="ascending_smallest_1st" \
	-DFILENAME2="ascending_smallest_2nd" -DFILENAME3="ascending_smallest_3rd" \
	-DFILE="ascending.smallest" gnuplot.gpi | gnuplot

# 4 threads
# summation
m4 -DTITLE="Sum up array" -DFILENAME1="4_summation_fastestandmedian" \
	-DFILENAME2="4_summation_slowest" -DFILENAME3="4_summation_average" \
	-DFILE="4_summation" singlethread.gpi | gnuplot
# ascending
m4 -DTITLE="Sum up array with ascending numbers" -DFILENAME1="4_ascending_fastestandmedian" \
	-DFILENAME2="4_ascending_slowest" -DFILENAME3="4_ascending_average" \
	-DFILE="4_ascending" singlethread.gpi | gnuplot


