-module(ebc_tinfo).
-include_lib("eunit/include/eunit.hrl").

-export([interval/1,
         min_interval/1,
         tracker_id/1,
         complete/1,
         incomplete/1,
         peers/1,
         compact_peers/1]).

interval(TInfo) ->
    {ok, Interval} = orddict:find(<<"interval">>, ebc:decode(TInfo)),
    Interval.

min_interval(TInfo) ->
    {ok, MinInterval} = orddict:find(<<"min interval">>, ebc:decode(TInfo)),
    MinInterval.

tracker_id(TInfo) ->
    case orddict:find(<<"tracker id">>, ebc:decode(TInfo)) of
        {ok, TrackerId} ->
            TrackerId;
        error ->
            undefined
    end.

complete(TInfo) ->
    {ok, Complete} = orddict:find(<<"complete">>, ebc:decode(TInfo)),
    Complete.

incomplete(TInfo) ->
    {ok, Incomplete} = orddict:find(<<"incomplete">>, ebc:decode(TInfo)),
    Incomplete.

peers(TInfo) ->
    {ok, EncodedPeers} = orddict:find(<<"peers">>, ebc:decode(TInfo)),
    compact_peers(EncodedPeers).

compact_peers(Bin) ->
    {PeerBins, <<>>} = ebc:unfold(fun(<<Peer:6/bytes, Rest/binary>>) ->
                                          {Peer, Rest};
                                     (<<>>) ->
                                          nothing
                                  end,
                                  Bin),
    [compact_peer(PeerBin) || PeerBin <- PeerBins].

compact_peer(<<Ip:4/bytes, HiPort, LoPort>>) ->
    IpTuple = list_to_tuple(binary_to_list(Ip)),
    Port = 256 * HiPort + LoPort,
    {IpTuple, Port}.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

compact_peer_test() ->
    ?assertEqual({{1,2,3,4}, 4567}, compact_peer(<<1,2,3,4,17,215>>)).

compact_peers_test() ->
    PeerA = {{1,2,3,4}, 1},
    PeerB = {{12,34,56,78}, 2},
    EncodedPeers = <<1,2,3,4,0,1,12,34,56,78,0,2>>,
    Peers = compact_peers(EncodedPeers),
    ?assert(lists:member(PeerA, Peers)),
    ?assert(lists:member(PeerB, Peers)).
