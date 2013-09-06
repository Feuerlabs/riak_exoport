-module(riak_exoport_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {connected}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, check_stats(maybe_connect(#st{}))}.



handle_call(_, _, S) ->
    {reply, error, S}.

handle_cast({connected, Node}, S) ->
    {noreply, S#st{connected = Node}};
handle_cast(_, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.

check_stats(#st{} = S) ->
    FolsomMetrics = folsom_metrics:get_metrics_info(),
    transaction(
      fun(_) ->
	      check_folsom_stats(FolsomMetrics)
      end),
    S.

transaction(F) ->
    %% This is horrid, but will do for now; kvdb locking coming soon...
    global:trans(
      {kvdb_conf,self()},
      fun() ->
	      kvdb_conf:transaction(F)
      end).

check_folsom_stats(Stats) ->
    Prefix = [<<"riak-stats">>, <<"node">>, atom_to_binary(node(), latin1)],
    lists:foreach(
      fun({Name, Info}) ->
	      Key = folsom_name_to_key(Name, Prefix),
	      case kvdb_conf:read(Key) of
		  {ok, {_, Opts, _}} ->
		      case lists:keyfind(user, 1, Opts) of
			  {_, true} ->
			      skip;
			  _ ->
			      %% should perhaps audit the entry?
			      write_folsom_metric(Key, Info, false)
		      end;
		  {error, _} ->
		      write_folsom_metric(Key, Info, false)
	      end
      end, Stats).

folsom_name_to_key(Name0, Prefix) ->
    Name = if is_tuple(Name0) -> tuple_to_list(Name0);
	      is_list(Name0)  -> Name0
	   end,
    kvdb_conf:join_key(Prefix ++ [atom_to_binary(A,latin1)
				  || A <- Name]).

write_folsom_metric(Key, Info, User) ->
    kvdb_conf:write({Key, [{user, User}], <<>>}),
    Type = atom_to_binary(proplists:get_value(type, Info), latin1),
    kvdb_conf:write_tree(
      Key, [{<<"node-type">>, [], folsom_node_type(Type)},
	    {<<"node-subtype">>, [], Type}]).

folsom_node_type(<<"meter">>) -> <<"data-point">>;
folsom_node_type(<<"counter">>) -> <<"data-point">>;
folsom_node_type(<<"gauge">>) -> <<"data-point">>;
folsom_node_type(_) -> <<"probe">>.

maybe_connect(#st{connected = N} = S) when N == node() ->
    S;
maybe_connect(#st{} = S) ->
    %% This is just a quick-and-dirty solution for now
    F = fun() -> check_connected(S) end,
    case global:trans({?MODULE, self()}, F, [node()|nodes()], 1) of
	aborted -> S;
	#st{} = S1 ->
	    S1
    end.

check_connected(S) ->
    kvdb_conf:in_transaction(
      fun(_) ->
	      case kvdb_conf:read(proxy_key()) of
		  {ok, {_, _, Node}} ->
		      case lists:member(Node, nodes()) of
			  true ->
			      S#st{connected = Node};
			  false ->
			      maybe_connect_(S)
		      end;
		  {error, not_found} ->
		      maybe_connect_(S)
	      end
      end).

maybe_connect_(S) ->
    case lists:sort([node() | nodes()]) of
	[N | _] when N == node() ->
	    connect(S);
	_ ->
	    S
    end.

connect(S) ->
    case exoport:ping() of
	{reply, pong, []} ->
	    kvdb_conf:write({proxy_key(), [], node()}),
	    gen_server:abcast(?MODULE, {connected, node()}),
	    S#st{connected = node()};
	_ ->
	    kvdb_conf:delete(proxy_key()),
	    S
    end.

proxy_key() ->
    <<"riak*exoport*proxy">>.
