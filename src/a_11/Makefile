FILES=collector.beam paxos.beam proposer.beam acceptor.beam kreator.beam
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
	erl -noinput -noshell -run test testcr $(NUMBER_OF_PROCESSES) -run init stop
	touch run

%.beam: %.erl
	erlc +debug_info $< 

# this assumes that the msc parser is given via $(MSCGEN)
create_graphs: 
	@for i in msc/*.msc; \
			do \
					echo "creating graph for $$i"; \
					$(MSCGEN) -i $$i -Tpng; \
			done

clean:
	-rm *.beam

.PHONY: run
