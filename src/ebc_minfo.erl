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
    {ok, Info} = orddict:find(<<"info">>, ebc:decode(Metainfo)),
    Info.

info_hash(Metainfo) ->
    Encoding = ebc:encode(info(Metainfo)),
    crypto:sha(Encoding).

announce(Metainfo) ->
    {ok, Announce} = orddict:find(<<"announce">>, ebc:decode(Metainfo)),
    binary_to_list(Announce).

pieces(Metainfo) ->
    {ok, Pieces} = orddict:find(<<"pieces">>, ebc:decode(info(Metainfo))),
    Pieces.

piece_length(Metainfo) ->
    {ok, PieceLength} = orddict:find(<<"piece length">>, ebc:decode(info(Metainfo))),
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

metainfo_test() ->
    {ok, Metainfo} = file:read_file("./test/test.torrent"),
    ?assertEqual("http://thomasballinger.com:6969/announce", announce(Metainfo)).
