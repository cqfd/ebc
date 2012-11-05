-module(ebc_peer_fsm).
-behaviour(gen_fsm).

-record(s, {conn,
            peer_id,
            interested=false}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         handshake/3,
         recv_handshake/4,
         keep_alive/1,
         recv_keep_alive/1,
         choke/1,
         recv_choke/1,
         unchoke/1,
         recv_unchoke/1,
         interested/1,
         recv_interested/1,
         not_interested/1,
         recv_not_interested/1,
         have/2,
         recv_have/2,
         bitfield/2,
         recv_bitfield/2,
         request/4,
         recv_request/4,
         piece/4,
         recv_piece/4,
         cancel/4,
         recv_cancel/4
        ]).

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

handshake(Pid, InfoHash, PeerId) ->
    gen_fsm:send_event(Pid, {handshake, InfoHash, PeerId}).
recv_handshake(Pid, Reserved, InfoHash, PeerId) ->
    gen_fsm:send_event(Pid, {recv_handshake, Reserved, InfoHash, PeerId}).

keep_alive(Pid) ->
    gen_fsm:send_all_state_event(Pid, keep_alive).
recv_keep_alive(Pid) ->
    gen_fsm:send_all_state_event(Pid, recv_keep_alive).

choke(Pid) ->
    gen_fsm:send_all_state_event(Pid, choke).
recv_choke(Pid) ->
    gen_fsm:send_event(Pid, recv_choke).
unchoke(Pid) ->
    gen_fsm:send_all_state_event(Pid, unchoke).
recv_unchoke(Pid) ->
    gen_fsm:send_all_state_event(Pid, recv_unchoke).

interested(Pid) ->
    gen_fsm:send_all_state_event(Pid, interested).
recv_interested(Pid) ->
    gen_fsm:send_event(Pid, recv_interested).
not_interested(Pid) ->
    gen_fsm:send_all_state_event(Pid, not_interested).
recv_not_interested(Pid) ->
    gen_fsm:send_event(Pid, recv_not_interested).

have(Pid, PieceIndex) ->
    gen_fsm:send_all_state_event(Pid, {have, PieceIndex}).
recv_have(Pid, PieceIndex) ->
    gen_fsm:send_event(Pid, {recv_have, PieceIndex}).

bitfield(Pid, Bitfield) ->
    gen_fsm:send_all_state_event(Pid, {bitfield, Bitfield}).
recv_bitfield(Pid, Bitfield) ->
    gen_fsm:send_event(Pid, {recv_bitfield, Bitfield}).

request(Pid, Index, Begin, Length) ->
    gen_fsm:send_all_state_event(Pid, {request, Index, Begin, Length}).
recv_request(Pid, Index, Begin, Length) ->
    gen_fsm:send_event(Pid, {recv_request, Index, Begin, Length}).

piece(Pid, Index, Begin, Block) ->
    gen_fsm:send_all_state_event(Pid, {piece, Index, Begin, Block}).
recv_piece(Pid, Index, Begin, Block) ->
    gen_fsm:send_event(Pid, {recv_piece, Index, Begin, Block}).

cancel(Pid, Index, Begin, Block) ->
    gen_fsm:send_all_state_event(Pid, {cancel, Index, Begin, Block}).
recv_cancel(Pid, Index, Begin, Block) ->
    gen_fsm:send_event(Pid, {recv_cancel, Index, Begin, Block}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Conn) ->
    {ok, inchoate, #s{conn=Conn}}.

inchoate({handshake, InfoHash, PeerId}, State=#s{conn=Conn}) ->
    Handshake = ebc_peer_protocol:handshake(InfoHash, PeerId),
    gen_tcp:send(Conn, Handshake),
    {next_state, shaking, State}.

shaking({recv_handshake, _Reserved, _InfoHash, PeerId}, State) ->
    io:format("Received handshake!~n"),
    {next_state, am_choking, State#s{peer_id=PeerId}}.

am_choking({recv_bitfield, Bitfield}, State) ->
    io:format("Received bitfield: ~p~n", [Bitfield]),
    {next_state, am_choking, State};
am_choking({recv_have, PieceIndex}, State) ->
    io:format("Peer has ~p~n", [PieceIndex]),
    {next_state, am_choking, State}.

handle_event(recv_keep_alive, StateName, State) ->
    {next_state, StateName, State};
handle_event(keep_alive, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:keep_alive()),
    {next_state, StateName, State};
handle_event(choke, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:choke()),
    {next_state, StateName, State};
handle_event(unchoke, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:unchoke()),
    {next_state, StateName, State};
handle_event(recv_unchoke, StateName, State) ->
    io:format("Peer has unchoked us!~n"),
    {next_state, StateName, State};
handle_event(interested, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:interested()),
    {next_state, StateName, State};
handle_event(not_interested, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:not_interested()),
    {next_state, StateName, State};
handle_event({have, PieceIndex}, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:have(PieceIndex)),
    {next_state, StateName, State};
handle_event({bitfield, Bitfield}, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:bitfield(Bitfield)),
    {next_state, StateName, State};
handle_event({request, Index, Begin, Length}, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:request(Index, Begin, Length)),
    {next_state, StateName, State};
handle_event({piece, Index, Begin, Block}, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:piece(Index, Begin, Block)),
    {next_state, StateName, State};
handle_event({cancel, Index, Begin, Length}, StateName, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:cancel(Index, Begin, Length)),
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
