-module(ebc_peer_fsm).
-behaviour(gen_fsm).
-include("ebc.hrl").

-record(s, {conn,
            peer_id,
            interested=false}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         send_msg/2,
         recv_msg/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([inchoate/2,
         shaking/2,
         am_choking/2]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Conn) ->
    gen_fsm:start_link(?MODULE, Conn, []).

-spec send_msg(pid(), bittorrent_msg()) -> 'ok'.
send_msg(Pid, Msg) ->
    gen_fsm:send_event(Pid, {send, Msg}).

-spec recv_msg(pid(), bittorrent_msg()) -> 'ok'.
recv_msg(Pid, Msg) ->
    gen_fsm:send_event(Pid, {recv, Msg}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Conn) ->
    {ok, inchoate, #s{conn=Conn}}.

inchoate({send, {handshake, Reserved, InfoHash, PeerId}}, State=#s{conn=Conn}) ->
    Handshake = ebc_peer_protocol:encode({handshake, Reserved, InfoHash, PeerId}),
    gen_tcp:send(Conn, Handshake),
    {next_state, shaking, State}.

shaking({recv, {handshake, _Reserved, _InfoHash, PeerId}}, State) ->
    io:format("Received handshake!~n"),
    {next_state, am_choking, State#s{peer_id=PeerId}}.

am_choking({recv, {bitfield, Bitfield}}, State) ->
    io:format("Received bitfield: ~p~n", [Bitfield]),
    {next_state, am_choking, State};

am_choking({recv, {have, PieceIndex}}, State) ->
    io:format("Peer has ~p~n", [PieceIndex]),
    {next_state, am_choking, State};

am_choking({send, keep_alive}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode(keep_alive)),
    {next_state, am_choking, State};
am_choking({recv, keep_alive}, State) ->
    {next_state, am_choking, State};

am_choking({send, choke}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode(choke)),
    {next_state, am_choking, State};

am_choking({send, unchoke}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode(unchoke)),
    {next_state, am_choking, State};
am_choking({recv, unchoke}, State) ->
    io:format("Peer has unchoked us!~n"),
    {next_state, am_choking, State};

am_choking({send, interested}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode(interested)),
    {next_state, am_choking, State};

am_choking({send, not_interested}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode(not_interested)),
    {next_state, am_choking, State};

am_choking({send, {have, PieceIndex}}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode({have,PieceIndex})),
    {next_state, am_choking, State};

am_choking({send, {bitfield, Bitfield}}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode({bitfield, Bitfield})),
    {next_state, am_choking, State};

am_choking({send, {request, Index, Begin, Length}}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode({request, Index, Begin, Length})),
    {next_state, am_choking, State};

am_choking({send, {piece, Index, Begin, Block}}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode({piece, Index, Begin, Block})),
    {next_state, am_choking, State};
am_choking({recv, {piece, Index, Begin, Block}}, State) ->
    io:format("Received piece ~p ~p: ~p~n", [Index, Begin, Block]),
    {next_state, am_choking, State};

am_choking({send, {cancel, Index, Begin, Length}}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode({cancel, Index, Begin, Length})),
    {next_state, am_choking, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
