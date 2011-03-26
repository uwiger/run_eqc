.PHONY: all compile clean eunit test test_eqc test_proper eqc doc

DIRS=src 
EQC=${HOME}/lib/eqc-1.0.1
PROP=${HOME}/git/proper

BRANCH=`git branch | awk '/\*/ {print $2}'`

all: get_deps compile

compile:
	./rebar compile

get_deps:
	./rebar get-deps

clean:
	./rebar clean

eunit:
	./rebar eunit

test/run_eqc_test.beam: test/run_eqc_test.erl
	erlc -W -o test test/run_eqc_test.erl

test/run_proper_test.beam: test/run_proper_test.erl
	erlc -W -o test test/run_proper_test.erl

test_eqc: script test/run_eqc_test.beam
	escript ./run_eqc.ez -m run_eqc_test -n 1000 -rpt error -pa test

test_proper: script test/run_proper_test.beam
	escript ./run_proper.ez -m run_proper_test -n 1000 \
	-rpt error -pa test

test: test_eqc test_proper

doc:
	./rebar doc

run_eqc.ez:
	escript ebin/run_eqc_gen.beam run_eqc.ez ${EQC}/ebin eqc

run_proper.ez:
	escript ebin/run_eqc_gen.beam run_proper.ez ${PROP}/ebin proper

script: run_eqc.ez run_proper.ez
#	escript ebin/run_eqc.beam generate run_eqc.escript ${EQC}
#	chmod u+x run_eqc.escript
#	escript ebin/run_proper.beam generate run_proper.escript ${PROP}
#	chmod u+x run_proper.escript
