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

handle_info({tcp, _Conn, Data}, State=#s{partial=P}) ->
    {Msgs, NewP} = ebc_peer_protocol:decode_many(<<P/bytes, Data/bytes>>),
    lists:foreach(fun(Msg) ->
                          ebc_peer_fsm:recv_msg(State#s.fsm, Msg)
                  end,
                  Msgs),
    {noreply, State#s{partial=NewP}};
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

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------
