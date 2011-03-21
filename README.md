

<h1>The run_eqc application</h1>

run_eqc - An escript wrapper for Quviq QuickCheck

==================================================
.

Copyright © 2010 Erlang Solutions Ltd.

__Version:__ 0.1



Run_eqc packages QuickCheck Mini as a single escript file and provides 
a command-line interface for running QuickCheck tests. with some 
additional support for output filtering.



The file `run_eqc.escript` should be a runnable version of the wrapper,
including `eqc-1.0.1` - the current version of EQC Mini. To build a new 
version, ensure that you have EQC Mini installed, update the location of
it in the Makefile, and call `make script`.


<pre>
Usage: run_eqc.escript [ Option ]
-n NumTests    run NumTests number of tests (default 100)
-m Module      run eqc:module([ Option, ] Module)
-pa Dir        prepend Dir to the code path
-pz Dir        append Dir to the code path
-rpt all       set reporting level - 'all': report everything
| none     'none': (as silent as possible)
| error    'error': report on failing tests.
</pre>




The `-rpt error` option buffers all output produced by QuickCheck and 
discards it for all successful properties. If a property fails, all output
for that run is printed.



Example:



Using the (broken) property in [run_eqc_test.erl](blob/master/test/run_eqc_test.erl), we can illustrate the use of num_tests (the error is unlikely to appear with only 100 tests), and `-rpt error`. We also note that `run_eqc.escript` signals test case failure appropriately to the make script.


<pre>
uwbook:run_eqc uwiger$ make test
./rebar compile
==> edown (compile)
==> run_eqc (compile)
escript ebin/run_eqc.beam generate run_eqc.escript /Users/uwiger/lib/eqc-1.0.1
chmod u+x run_eqc.escript
erlc -W -o test test/run_eqc_test.erl
./run_eqc.escript -m run_eqc_test -n 1000 -rpt error -pa test
Starting eqc mini version 1.0.1 (compiled at {{2010,6,13},{11,15,30}})
FAILED property prop_lists_delete:
....................................................................................................................................................................................................................................................Failed! After 245 tests.
{-12,[19,-25,24,-12,13,22,18,-12,-2]}
Shrinking.....(5 times)
{-12,[-12,-12]}
Failed properties in run_eqc_test:
[prop_lists_delete]
make: *** [test] Error 1
</pre>




If we test the same module using default values:


<pre>
uwbook:run_eqc uwiger$ ./run_eqc.escript -m run_eqc_test -pa test/
Starting eqc mini version 1.0.1 (compiled at {{2010,6,13},{11,15,30}})
....................................................................................................
OK, passed 100 tests
....................................................................................................
OK, passed 100 tests
....................................................................................................
OK, passed 100 tests
...tests passed (run_eqc_test)
</pre>



<h2 class="indextitle">Modules</h2>



<table width="100%" border="0" summary="list of modules">
<tr><td><a href="blob/master/doc/run_eqc.md" class="module">run_eqc</a></td></tr>
<tr><td><a href="blob/master/doc/run_eqc_app.md" class="module">run_eqc_app</a></td></tr>
<tr><td><a href="blob/master/doc/run_eqc_sup.md" class="module">run_eqc_sup</a></td></tr></table>

