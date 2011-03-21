%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%% @doc escript wrapper for Quviq QuickCheck.

-module(run_eqc).
-export([main/1]).
-export([generate/2,
	 load/1,
	 i/1]).


main(["generate", Out, EqcMini]) ->
    generate(Out, EqcMini);
main(S) ->
    Args = try args(S)
	   catch
	       error:R ->
		   usage(R)
	   end,
    N = proplists:get_value(numtests, Args, 100),
    M = proplists:get_value(module, Args),
    Rpt = proplists:get_value(rpt, Args, all),
    Output = case Rpt of
		 none ->
		     fun(_, _) -> ok end;
		 all ->
		     fun(Str, IOArgs) ->
			     io:format(user, Str, IOArgs)
		     end;
		 error ->
		     fun(Str, IOArgs) ->
			     push_io({Str, IOArgs})
		     end
	     end,
    case module([{numtests, N},
		 {on_output, Output}], Rpt, M) of
		 %% {on_test, fun on_test/2}], M) of
	[] ->
	    io:fwrite("...tests passed (~p)~n", [M]),
	    ok;
	Failed ->
	    
	    io:fwrite("Failed properties in ~p:~n"
		      "~p~n", [M, Failed]),
	    erlang:halt(1)
    end.

usage(Error) ->
    io:fwrite(
      "Error: ~p~n"
      "Usage: ~s [ Option ]~n"
      "  -n NumTests    run NumTests number of tests (default 100)~n"
      "  -m Module      run eqc:module([ Option, ] Module)~n"
      "  -pa Dir        prepend Dir to the code path~n"
      "  -pz Dir        append Dir to the code path~n"
      "  -rpt all       set reporting level - 'all': report everything (default)~n"
      "      | none     'none': (as silent as possible)~n"
      "      | error    'error': report on failing tests.~n",
      [Error, script_name()]),
    erlang:halt(1).

script_name() ->
    try filename:basename(escript:script_name())
    catch
	error:_ ->
	    ?MODULE_STRING
    end.

args(["-n", Ns|T]) ->
    [{numtests, list_to_integer(Ns)}|args(T)];
args(["-pa", P|T]) ->
    code:add_patha(P),
    args(T);
args(["-pz", P|T]) ->
    code:add_pathz(P),
    args(T);
args(["-m", Ms|T]) ->
    [{module, list_to_atom(Ms)}|args(T)];
args(["-rpt", Flag | T]) ->
    Level = case Flag of
		"all" -> all;
		"none" -> none;
		"error" -> error
	    end,
    [{rpt, Level}|args(T)];
args([]) ->
    [].

%% @spec generate(EscriptName, EqcMiniDir) -> ok
%% @doc Creates the escript file, using a local installation of EQC Mini
%%
%% This function packages the EQC Mini application together with the `run_eqc'
%% wrapper into one single escript file. `EqcMiniDir' should point to an 
%% existing installation of EQC Mini.
%% @end
%%
generate(Out, EqcMini) ->
    F = fun(F,Acc) ->
		{ok,Bin} = file:read_file(F),
		[{filename:basename(F),Bin}|Acc]
	end,
    Acc1 = filelib:fold_files(
	     EqcMini ++ "/ebin",
	     ".*\\.beam",
	     false, F, []),
    This = code:which(?MODULE),
    {ok, Bin} = file:read_file(code:which(?MODULE)),
    escript:create(
      Out, [shebang,
	    {archive, [{filename:basename(This), Bin} | Acc1], []}]).

%% @spec i(Escript) -> pid()
%% @doc Load and run EQC Mini interactively from the Escript file
%%
%% This function is intended to be called from within an Erlang node.
%% It loads EQC Mini {@link load/1}, and calls `eqc:start()'.
%% @end
%%
i(Escript) ->
    ok = load(Escript),
    eqc:start().

%% @spec load(Escript) -> ok
%% @doc Loads the EQC Mini modules directly from the Escript file
%%
%% This function is intended to be called from within a running Erlang node.
%% It loads the EQC Mini byte code directly from the escript file, and makes
%% it possible to use EQC Mini interactively.
%% @end
%%
load(Escript) ->
    case escript:extract(Escript, []) of
	{ok, PList} ->
	    case zip:extract(proplists:get_value(archive, PList), [memory]) of
		{ok, Files} ->
		    lists:foreach(
		      fun({Fname, Bin}) ->
			      Module = list_to_atom(
					 filename:basename(Fname, ".beam")),
			      {module, _} = code:load_binary(Module, Fname, Bin)
		      end, Files);
		Err1 ->
		    Err1
	    end;
	Err0 ->
	    Err0
    end.
    
module(Opt, Rpt, Mod) ->
    case erlang:function_exported(eqc, module, 2) of
	true ->
	    eqc:module(Opt, Mod);
	false ->
	    case Opt == [] of
		true -> eqc:module(Mod);
		false ->
		    module2(Opt, Rpt, Mod)
	    end
    end.

module2(Opt, Rpt, Mod) ->
    Exports = Mod:module_info(exports),
    Props = [F || {F, 0} <- Exports,
		  is_prop(F)],
    Apply = if is_list(Opt) ->
		    fun(F) ->
			    lists:foldl(fun({OptF,Arg}, Fx) ->
						eqc:OptF(Arg, Fx)
					end, F, Opt)
		    end;
	       true ->
		    {OptF, Arg} = Opt,
		    fun(F) -> eqc:OptF(Arg, F()) end
	    end,
    if Props =/= [], Rpt == error ->
	    spawn_reporter();
       true ->
	    ok
    end,
    Res = lists:filter(fun(P) ->
			       new_context(P, Rpt),
			       Passed = eqc:quickcheck(Apply(Mod:P())),
			       clear_context(Passed, Rpt),
			       not Passed
		       end, Props),
    close_reporter(),
    Res.

is_prop(F) ->
    case atom_to_list(F) of
	"prop_" ++ _ -> true;
	_ -> false
    end.

spawn_reporter() ->
    Parent = self(),
    P = spawn(fun() ->
		      register(run_eqc_reporter, self()),
		      erlang:monitor(process, Parent),
		      Parent ! {ok, self()},
		      reporter_loop(undefined, queue:new())
	      end),
    receive
	{ok, P} = Res ->
	    Res
    end.

close_reporter() ->
    case whereis(run_eqc_reporter) of
	undefined ->
	    ok;
	P ->
	    Ref = erlang:monitor(process, P),
	    P ! {self(), stop},
	    receive
		{P, ok} ->
		    ok;
		{'DOWN', Ref, _, _, _} ->
		    ok
	    after 10000 ->
		    exit(P, kill),
		    ok
	    end
    end.
	    

reporter_loop(P, Q) ->
    receive
	{'DOWN', _, _, _, _} ->
	    ok;
	{Pid, stop} ->
	    Pid ! {self(), ok};
	{new_context, P1} ->
	    reporter_loop(P1, queue:new());
	{output, IO} ->
	    reporter_loop(P, queue:in(IO, Q));
	report ->
	    io:fwrite("FAILED property ~p:~n", [P]),
	    print_queue(queue:out(Q)),
	    reporter_loop(undefined, queue:new())
    end.

new_context(P, error) ->
    run_eqc_reporter ! {new_context, P};
new_context(_, _) ->
    ok.

clear_context(false, error) ->
    run_eqc_reporter ! report;
clear_context(_, _) ->
    ok.

print_queue({{value, {Str,Args}}, Q}) ->
    io:fwrite(Str, Args),
    print_queue(queue:out(Q));
print_queue(_) ->
    ok.
	    
push_io(IO) ->
    catch run_eqc_reporter ! {output, IO}.

