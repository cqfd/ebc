-module(ebc_minfo).
-include_lib("eunit/include/eunit.hrl").

-export([info/1,
         info_hash/1,
         announce/1,
         pieces/1,
         piece_length/1,
         name/1,
         width/1 % aka length
        ]).

info(Metainfo) ->
    {Decoding, <<>>} = ebc:decode(Metainfo),
    {ok, Info} = orddict:find(<<"info">>, Decoding),
    Info.

info_hash(Metainfo) ->
    Encoding = ebc:encode(info(Metainfo)),
    crypto:sha(Encoding).

announce(Metainfo) ->
    {Decoding, <<>>} = ebc:decode(Metainfo),
    {ok, Announce} = orddict:find(<<"announce">>, Decoding),
    binary_to_list(Announce).

pieces(Metainfo) ->
    {ok, Pieces} = orddict:find(<<"pieces">>, info(Metainfo)),
    {Hashes, <<>>} = ebc:unfold(fun(<<Piece:20/bytes, Rest/bytes>>) ->
                                        {Piece, Rest};
                                   (<<>>) ->
                                        nothing
                                end,
                                Pieces),
    Hashes.

piece_length(Metainfo) ->
    {ok, PieceLength} = orddict:find(<<"piece length">>, info(Metainfo)),
    PieceLength.

name(Metainfo) ->
    {ok, Name} = orddict:find(<<"name">>, info(Metainfo)),
    binary_to_list(Name).

width(Metainfo) ->
    {ok, Length} = orddict:find(<<"length">>, info(Metainfo)),
    Length.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

metainfo_announce_test() ->
    {ok, Metainfo} = file:read_file("./test/test.torrent"),
    ?assertEqual("http://thomasballinger.com:6969/announce", announce(Metainfo)).

metainfo_length_test() ->
    {ok, Metainfo} = file:read_file("./test/test.torrent"),
    ?assertEqual(1751391, width(Metainfo)).
