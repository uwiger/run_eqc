#!/usr/bin/env escript
%% -*- erlang -*-

main([Src, Target]) ->
    Branch = get_git_branch(),
    case file:read_file(Src) of
	{ok, SrcBin} ->
	    SrcStr = <<"href=\"">>,
	    NewStr = list_to_binary("href=\"target/" ++ Branch ++ "/doc/"),
	    TgtBin = binary:replace(SrcBin,
				    SrcStr,
				    NewStr, [global]
			       ),
	    ok = file:write_file(Target, TgtBin)
    end.


get_git_branch() ->
    case os:cmd("git branch | awk '/\\*/ {print $2}'") of
	[_,_|_] = Res ->
	    %% trailing newline expected - remove.
	    lists:reverse(tl(lists:reverse(Res)));
	Other ->
	    io:fwrite("Could not get git branch (~s)~n", [Other]),
	    halt(1)
    end.
