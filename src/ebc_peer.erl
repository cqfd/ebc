-module(ebc_peer).
-include_lib("eunit/include/eunit.hrl").

-export([start_shooker/1,
         start_shaker/4]).

start_shooker(Conn) ->
    {ok, Sup} = ebc_peers_sup:start_child(Conn),
    {ok, Fsm} = ebc_peer_sup:get_fsm(Sup),
    {ok, Reader} = ebc_peer_sup:start_reader(Sup, Conn, Fsm),
    gen_tcp:controlling_process(Conn, Reader),
    inet:setopts(Conn, [{active, true}]),
    {ok, Fsm}.

start_shaker(IpTuple, Port, InfoHash, PeerId) ->
    case gen_tcp:connect(IpTuple, Port, [binary, {active, false}]) of
        {ok, Conn} ->
            {ok, Sup} = ebc_peers_sup:start_child(Conn),
            {ok, Fsm} = ebc_peer_sup:get_fsm(Sup),
            {ok, Reader} = ebc_peer_sup:start_reader(Sup, Conn, Fsm),
            gen_tcp:controlling_process(Conn, Reader),
            inet:setopts(Conn, [{active, true}]),
            ebc_peer_fsm:handshake(Fsm, InfoHash, PeerId),
            {ok, Fsm};
        Error ->
            Error
    end.
