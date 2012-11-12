-module(ebc_peer_fsm).
-behaviour(gen_fsm).
-include("ebc.hrl").

-record(s, {conn,
            peer_id}).

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

inchoate({send, H = {handshake, _, _, _}}, State=#s{conn=Conn}) ->
    gen_tcp:send(Conn, ebc_peer_protocol:encode(H)),
    {next_state, shaking, State}.

shaking({recv, {handshake, _Reserved, _InfoHash, PeerId}}, State) ->
    io:format("Received handshake!~n"),
    {next_state, am_choking, State#s{peer_id=PeerId}}.

am_choking({send, Msg}, State) ->
    gen_tcp:send(State#s.conn, ebc_peer_protocol:encode(Msg)),
    {next_state, am_choking, State};

am_choking({recv, keep_alive}, State) ->
    {next_state, am_choking, State};

am_choking({recv, choke}, State) ->
    ebc_brain:choke(self()),
    {next_state, am_choking, State};
am_choking({recv, unchoke}, State) ->
    ebc_brain:unchoke(self()),
    {next_state, am_choking, State};

am_choking({recv, interested}, State) ->
    {next_state, am_choking, State};
am_choking({recv, not_interested}, State) ->
    {next_state, am_choking, State};

am_choking({recv, {have, PieceIndex}}, State) ->
    io:format("Peer has ~p~n", [PieceIndex]),
    {next_state, am_choking, State};

am_choking({recv, {bitfield, Bitfield}}, State) ->
    io:format("Received bitfield: ~p~n", [Bitfield]),
    {next_state, am_choking, State};

am_choking({recv, {request, Index, Begin, Length}}, State) ->
    io:format("Received request: ~p, ~p, ~p~n", [Index, Begin, Length]),
    {next_state, am_choking, State};

am_choking({recv, {piece, Index, Begin, Block}}, State) ->
    io:format("Received piece ~p ~p: ~p~n", [Index, Begin, Block]),
    {next_state, am_choking, State};

am_choking({recv, {cancel, Index, Begin, Length}}, State) ->
    io:format("Received cancel: ~p, ~p, ~p~n", [Index, Begin, Length]),
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
