COLLECTSPATH=$(shell racket -e '(require setup/dirs)' -e '(display (path->string (find-collects-dir)))')

RACOEXEFLAGS=--collects-path $(shell readlink -m $(COLLECTSPATH)) --exf-clear ++exf -m ++exf -U ++exf --

.PHONY : all
all : mceval

.PHONY : clean
clean :
	rm -rf compiled mceval run-tests

mceval : mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

run-tests : run-tests.rkt mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

.PHONY : test
test : run-tests
	./run-tests
