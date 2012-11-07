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
    {Decoding, <<>>} = ebc:decode(TInfo),
    {ok, Interval} = orddict:find(<<"interval">>, Decoding),
    Interval.

min_interval(TInfo) ->
    {Decoding, <<>>} = ebc:decode(TInfo),
    {ok, MinInterval} = orddict:find(<<"min interval">>, Decoding),
    MinInterval.

tracker_id(TInfo) ->
    {Decoding, <<>>} = ebc:decode(TInfo),
    case orddict:find(<<"tracker id">>, Decoding) of
        {ok, TrackerId} ->
            TrackerId;
        error ->
            undefined
    end.

complete(TInfo) ->
    {Decoding, <<>>} = ebc:decode(TInfo),
    {ok, Complete} = orddict:find(<<"complete">>, Decoding),
    Complete.

incomplete(TInfo) ->
    {Decoding, <<>>} = ebc:decode(TInfo),
    {ok, Incomplete} = orddict:find(<<"incomplete">>, Decoding),
    Incomplete.

peers(TInfo) ->
    {Decoding, <<>>} = ebc:decode(TInfo),
    {ok, EncodedPeers} = orddict:find(<<"peers">>, Decoding),
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
