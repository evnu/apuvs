GRAPHDIR=graphs

all: $(GRAPHDIR)/example1.pdf $(GRAPHDIR)/example2.pdf $(GRAPHDIR)/example3.pdf $(GRAPHDIR)/example4.pdf \
		$(GRAPHDIR)/example5.pdf \
		$(GRAPHDIR)/example6.pdf \
		$(GRAPHDIR)/example7.pdf

$(GRAPHDIR)/%.pdf: %.dot
	dot $< -Tpdf > $@
