.PHONY: all compile clean eunit test eqc doc

DIRS=src 
EQC=${HOME}/lib/eqc-1.0.1

all: compile

compile:
	./rebar compile


clean:
	./rebar clean

eunit:
	./rebar eunit

test: eunit

doc:
	./rebar doc

script: compile
	escript ebin/run_eqc.beam generate run_eqc.escript ${EQC}
	chmod u+x run_eqc.escript