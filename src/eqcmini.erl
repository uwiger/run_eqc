-module(eqcmini).
-export([main/1]).
-export([generate/2]).


main(["generate", Out, EqcMini]) ->
    generate(Out, EqcMini);
main(S) ->
    Args = args(S),
    N = proplists:get_value(numtests, Args, 100),
    M = proplists:get_value(module, Args),
    Output = case proplists:get_value(silent, Args, false) of
		 true -> fun(_, _) -> ok end;
		 false ->
		     fun(Str, IOArgs) ->
			     io:format(user, Str, IOArgs)
		     end
	     end,
    io:fwrite("running tests...~n", []),
    case module([{numtests, N}, {on_output, Output}], M) of
	[] ->
	    io:fwrite("...tests passed~n", []),
	    ok;
	Failed ->
	    
	    io:fwrite("Failed properties in ~p:~n"
		      "~p~n", [M, Failed]),
	    erlang:halt(1)
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
args(["-silent"|T]) ->
    [{silent, true}|args(T)];
args([]) ->
    [].


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
    
module(Opt, Mod) ->
    case erlang:function_exported(eqc, module, 2) of
	true ->
	    eqc:module(Opt, Mod);
	false ->
	    case Opt == [] of
		true -> eqc:module(Mod);
		false ->
		    module2(Opt, Mod)
	    end
    end.

module2(Opt, Mod) ->
    Exports = Mod:module_info(exports),
    Props = [F || {F, 0} <- Exports,
		  is_prop(F)],
    io:fwrite("Props = ~p~n", [Props]),
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
    lists:filter(fun(P) ->
			 not(eqc:quickcheck(Apply(Mod:P())))
		 end, Props).

is_prop(F) ->
    case atom_to_list(F) of
	"prop_" ++ _ -> true;
	_ -> false
    end.
	    
