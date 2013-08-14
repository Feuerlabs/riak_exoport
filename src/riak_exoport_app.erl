-module(riak_exoport_app).

-behaviour(application).

-export([start/2,
	 start_phase/3,
	 stop/1]).

-spec start(StartType:: normal |
                        {takeover, Node::atom()} |
                        {failover, Node::atom()},
            StartArgs::term()) ->
                   {ok, Pid::pid()} |
                   {ok, Pid::pid(), State::term()} |
                   {error, Reason::term()}.
start(_StartType, _StartArgs) ->
    kvdb:await(kvdb_conf),
    riak_exoport_sup:start_link().

start_phase(_Phase, _StartType, _Args) ->
    ok.

stop(_State) ->
    ok.

