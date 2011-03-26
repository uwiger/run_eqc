-module(run_eqc_gen).
-export([main/1]).


main([Archive, QC, Which]) when Which=="proper"; Which=="eqc" ->
    QCPath = Which ++ "/" ++ "ebin" ++ "/",
    QCFiles = 
	filelib:fold_files(
	  QC, "\\.beam\$", false, 
	  fun(F, Acc) ->
		  {ok, Bin} = file:read_file(F),
		  FN = QCPath ++ filename:basename(F),
		  [{FN, Bin}|Acc]
	  end, []),
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
