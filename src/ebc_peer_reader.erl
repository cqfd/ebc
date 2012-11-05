-module(ebc_peer_reader).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-record(s, {conn,
            fsm,
            partial = <<>>}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Conn, Fsm) ->
    gen_server:start_link(?MODULE, [Conn, Fsm], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Conn, Fsm]) ->
    {ok, #s{conn=Conn, fsm=Fsm}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Conn, Data}, State=#s{partial=Partial}) ->
    io:format("tcp data: ~p~n", [Data]),
    {Decodings, NewPartial} = decode(<<Partial/bytes, Data/bytes>>),
    lists:foreach(fun(D) ->
                          forward_decoding(State#s.fsm, D)
                  end,
                  Decodings),
    {noreply, State#s{partial=NewPartial}};
handle_info({tcp_closed, _Conn}, State) ->
    {stop, tcp_closed, State};
handle_info({tcp_error, _Conn, Reason}, State) ->
    io:format("tcp_error: ~p~n", [Reason]),
    {stop, tcp_error, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

forward_decoding(Fsm, {handshake, Reserved, InfoHash, PeerId}) ->
    ebc_peer_fsm:recv_handshake(Fsm, Reserved, InfoHash, PeerId);
forward_decoding(Fsm, keep_alive) ->
    ebc_peer_fsm:recv_keep_alive(Fsm);
forward_decoding(Fsm, choke) ->
    ebc_peer_fsm:recv_choke(Fsm);
forward_decoding(Fsm, unchoke) ->
    ebc_peer_fsm:recv_unchoke(Fsm);
forward_decoding(Fsm, interested) ->
    ebc_peer_fsm:recv_interested(Fsm);
forward_decoding(Fsm, not_interested) ->
    ebc_peer_fsm:recv_not_interested(Fsm);
forward_decoding(Fsm, {have, PieceIndex}) ->
    ebc_peer_fsm:recv_have(Fsm, PieceIndex);
forward_decoding(Fsm, {bitfield, Bitfield}) ->
    ebc_peer_fsm:recv_bitfield(Fsm, Bitfield);
forward_decoding(Fsm, {request, Index, Begin, Length}) ->
    ebc_peer_fsm:recv_request(Fsm, Index, Begin, Length);
forward_decoding(Fsm, {piece, Index, Begin, Length}) ->
    ebc_peer_fsm:recv_piece(Fsm, Index, Begin, Length);
forward_decoding(Fsm, {cancel, Index, Begin, Block}) ->
    ebc_peer_fsm:recv_cancel(Fsm, Index, Begin, Block);
forward_decoding(_Fsm, Decoding) ->
    io:format("Decoding: ~p~n", [Decoding]).

decode(Bin) ->
    decode(Bin, [], <<>>).
decode(<<>>, Decodings, Partial) ->
    {lists:reverse(Decodings), Partial};
decode(Bin, Decodings, <<>>) ->
    try ebc_peer_protocol:decode(Bin) of
        {Decoding, Rest} ->
            decode(Rest, [Decoding|Decodings], <<>>)
    catch
        error:_Msg ->
            {lists:reverse(Decodings), Bin}
    end.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

decode_test() ->
    Input = <<"li1337e6:foobare10:bittorren">>,
    Decodings = [[1337, <<"foobar">>]],
    Rest = <<"10:bittorren">>,
    ?assertEqual({Decodings, Rest}, decode(Input)).
