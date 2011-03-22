-module(run_proper_test).
-compile(export_all).
-include_lib("proper/include/proper.hrl").


%% meant to succeed
prop_lists_reverse() ->
    ?FORALL(L, list(int()),
	    lists:reverse(lists:reverse(L)) == L).

%% meant to succeed
prop_lists_member() ->
    ?FORALL({I, L}, {int(), list(int())},
	    lists:member(I, L ++ [I]) == true).

%% meant to fail (but usually not within 100 tests)
prop_lists_delete() ->
    ?FORALL({I, L}, {int(), list(int())},
	    lists:delete(I, L) == [J || J <- L,
					J =/= I]).
