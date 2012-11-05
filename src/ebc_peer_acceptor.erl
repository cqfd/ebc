-module(ebc_peer_acceptor).
-export([start_link/0, loop/1]).

start_link() ->
    {ok, LSock} = gen_tcp:listen(6881, [binary, {active, false}, {reuseaddr, true}]),
    register(?MODULE, proc_lib:spawn_link(?MODULE, loop, [LSock])),
    {ok, whereis(?MODULE)}.

loop(LSock) ->
    {ok, Conn} = gen_tcp:accept(LSock),
    ebc_peer:start_shooker(Conn),
    loop(LSock).
