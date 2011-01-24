FILES=changrob.beam collector.beam test.beam
MSCGEN?=/home/evnu/tmp/msc/mscgen-0.19/src/mscgen

all: $(FILES) .check

.check: $(FILES)
	@for i in $?; \
	do \
		echo "Checking $$i"; \
		dialyzer $$i;\
	done 
	touch .check

run: all
	erl -noinput -noshell -run test testcr 5 -run init stop

%.beam: %.erl
	erlc +debug_info $< 

# this assumes that the msc parser is given via $(MSCGEN)
create_graphs: 
	@for i in msc/*; \
			do \
					echo "creating graph for $$i"; \
					$(MSCGEN) -i $$i -Tpng; \
			done

clean:
	-rm *.beam
