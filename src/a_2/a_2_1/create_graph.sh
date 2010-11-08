#!/bin/bash
# Fajerski, Müller, Warnke - G02
#
# create graphs from data in data/ using gnuplot template gnuplot.gpi
#
# Requirements:
#		* gnuplot
#   * m4


m4 -DDISTRIBUTED="fixedarray_1000000_blockingsendtype_distributed" \
	 -DSINGLEMACHINE="fixedarray_1000000_blockingsendtype_singlemachine" \
	 -DTITLE="Blocking Send" \
	 -DFILENAME="blockingsend" gnuplot.gpi | gnuplot

m4 -DDISTRIBUTED="fixedarray_1000000_defaultsendtype_distributed" \
	 -DSINGLEMACHINE="fixedarray_1000000_defaultsendtype_singlemachine" \
	 -DTITLE="Default Send" \
	 -DFILENAME="defaultsendtype" gnuplot.gpi | gnuplot

m4 -DDISTRIBUTED="fixedarray_1000000_readysendtype_distributed" \
	 -DSINGLEMACHINE="fixedarray_1000000_readysendtype_singlemachine" \
	 -DTITLE="Ready Send" \
	 -DFILENAME="readysendtype" gnuplot.gpi | gnuplot

m4 -DDISTRIBUTED="fixedarray_1000000_synchronsendtype_distributed" \
	 -DSINGLEMACHINE="fixedarray_1000000_synchronsendtype_singlemachine" \
	 -DTITLE="Synchronous Send" \
	 -DFILENAME="synchronsendtype" gnuplot.gpi | gnuplot
