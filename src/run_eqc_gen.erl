-module(run_eqc_gen).
-export([main/1]).


main([Archive, QC, Which]) when Which=="proper"; Which=="eqc" ->
    QCPath = Which ++ "/" ++ "ebin" ++ "/",
    QCIncl = Which ++ "/" ++ "include" ++ "/",
    QCFiles = 
	fold_files(join(QC,"include"), QCIncl) ++
	fold_files(join(QC,"ebin"), QCPath),
    MyPath = filename:dirname(code:which(?MODULE)),
    MainMod = case Which of
		  "eqc" ->
		      MyPath ++ "/run_eqc.beam";
		  "proper" ->
		      MyPath ++ "/run_proper.beam"
	      end,
    {ok, Bin} = file:read_file(MainMod),
    ZipName = "run_eqc/ebin/" ++ filename:basename(MainMod),
    zip:create(Archive, [{ZipName, Bin}
			 | QCFiles]).

file_bin(F) ->
    {ok, B} = file:read_file(F),
    B.

fold_files(D, Path) ->
    filelib:fold_files(
      D, ".*", false, 
      fun(F, Acc) ->
	      FN = Path ++ filename:basename(F),
	      [{FN, file_bin(F)}|Acc]
      end, []).

join(A, B) ->
    filename:join(A, B).
