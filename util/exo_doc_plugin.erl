%% Rebar plugin for managing yang files
-module(exo_doc_plugin).

-export([doc/2,
	 clean/2]).

-spec doc(rebar_config:config(), _AppFile::file:filename()) -> 'ok'.
doc(Config, _AppFile) ->
    rebar_base_compiler:run(Config, [], "yang", ".yang", "doc", ".md",
			    fun mk_markdown/3, []).

-spec mk_markdown(file:filename(), file:filename(),
		  rebar_config:config()) -> ok.
mk_markdown(Source, Target, Config) ->
    try mk_markdown_(Source, Target, Config)
    catch
	error:E ->
	    io:fwrite("ERROR ~p~n~p~n", [E, erlang:get_stacktrace()]),
	    throw(E)
    end.

mk_markdown_(Source, Target, Config) ->
    io:fwrite("~p:mk_markdown(~p, ...)~n", [?MODULE,Source]),
    ok = filelib:ensure_dir(Target),
    case yang_json:json_rpc(Source, yang_opts(Config)) of
	{error, _} = Err ->
	    io:fwrite("yang:json_rpc(~p) -> ~p~n", [Source,Err]),
	    throw(Err);
	JSON when is_list(JSON) ->
	    yang_json:markdown(Target, JSON)
    end.

-spec clean(rebar_config:config(), _AppFile::file:filename()) -> 'ok'.
clean(_Config, _AppFile) ->
    Files = filelib:wildcard("doc/*.md"),
    ok = rebar_file_utils:delete_each(Files).

yang_opts(Config) ->
    rebar_config:get(Config, yang_opts, []).
