.PHONY: all compile clean eunit test test_eqc test_proper eqc doc

DIRS=src 
EQC=${HOME}/lib/eqc-1.0.1
PROP=${HOME}/git/proper

BRANCH=`git branch | awk '/\*/ {print $2}'`

all: compile

compile:
	./rebar compile


clean:
	./rebar clean

eunit:
	./rebar eunit

test/run_eqc_test.beam: test/run_eqc_test.erl
	erlc -W -o test test/run_eqc_test.erl

test/run_proper_test.beam: test/run_proper_test.erl
	erlc -W -o test test/run_proper_test.erl

test_eqc: script test/run_eqc_test.beam
	./run_eqc.escript -m run_eqc_test -n 1000 -rpt error -pa test

test_proper: script test/run_proper_test.beam
	./run_proper.escript -m run_proper_test -n 1000 -rpt error -pa test

test: test_eqc test_proper

doc:
	./rebar doc
	./mk_readme.escript doc/README.md README.md

script: compile
	escript ebin/run_eqc.beam generate run_eqc.escript ${EQC}
	chmod u+x run_eqc.escript
	escript ebin/run_proper.beam generate run_proper.escript ${PROP}
	chmod u+x run_proper.escript