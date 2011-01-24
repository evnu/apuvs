FILES=changrob.beam collector.beam test.beam

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

clean:
	-rm *.beam
