-module(ebc_peer_protocol).
-include_lib("eunit/include/eunit.hrl").

-export([decode/1,
         handshake/2,
         keep_alive/0,
         choke/0,
         unchoke/0,
         interested/0,
         not_interested/0,
         have/1,
         bitfield/1,
         request/3,
         piece/3,
         cancel/3
        ]).

decode(<<19, "BitTorrent protocol",
         Reserved:8/bytes, InfoHash:20/bytes, PeerId:20/bytes,
         Rest/bytes>>) ->
    {{handshake, Reserved, InfoHash, PeerId}, Rest};
decode(<<0:32, Rest/binary>>) ->
    {keep_alive, Rest};
decode(<<Len:4/bytes, Bin/bytes>>) ->
    Int = btoi(Len),
    <<Decoding:Int/bytes, Rest/bytes>> = Bin,
    decode_by_id(Decoding, Rest).

handshake(InfoHash, PeerId) ->
    <<19, "BitTorrent protocol", 0:64, InfoHash/bytes, PeerId/bytes>>.
keep_alive() ->
    <<0,0,0,0>>.
choke() ->
    <<0,0,0,1,0>>.
unchoke() ->
    <<0,0,0,1,1>>.
interested() ->
    <<0,0,0,1,2>>.
not_interested() ->
    <<0,0,0,1,3>>.
have(PieceIndex) ->
    <<0,0,0,5,4,(itob(PieceIndex))>>.
bitfield(Bitfield) ->
    Len = byte_size(Bitfield),
    LenBytes = itob(Len + 1),
    <<LenBytes/bytes, 5, Bitfield/bytes>>.
request(Index, Begin, Length) ->
    <<0,0,0,13,6, (itob(Index))/bytes, (itob(Begin))/bytes, (itob(Length))/bytes>>.
piece(Index, Begin, Block) ->
    <<0,0,0,9,7, (itob(Index))/bytes, (itob(Begin))/bytes, Block/bytes>>.
cancel(Index, Begin, Length) ->
    <<0,0,0,13,8, (itob(Index))/bytes, (itob(Begin))/bytes, (itob(Length))/bytes>>.

decode_by_id(<<0>>, Rest) ->
    {choke, Rest};
decode_by_id(<<1>>, Rest) ->
    {unchoke, Rest};
decode_by_id(<<2>>, Rest) ->
    {interested, Rest};
decode_by_id(<<3>>, Rest) ->
    {not_interested, Rest};
decode_by_id(<<4, Index:4/bytes>>, Rest) ->
    {{have, btoi(Index)}, Rest};
decode_by_id(<<5, Bitfield/bytes>>, Rest) ->
    {{bitfield, Bitfield}, Rest};
decode_by_id(<<6, Index:4/bytes, Begin:4/bytes, Length:4/bytes>>, Rest) ->
    {{request, btoi(Index), btoi(Begin), btoi(Length)}, Rest};
decode_by_id(<<7, Index:4/bytes, Begin:4/bytes, Block/bytes>>, Rest) ->
    {{piece, btoi(Index), btoi(Begin), Block}, Rest};
decode_by_id(<<8, Index:4/bytes, Begin:4/bytes, Length:4/bytes>>, Rest) ->
    {{cancel, btoi(Index), btoi(Begin), btoi(Length)}, Rest}.

btoi(<<A, B, C, D>>) ->
    D + 256 * (C + 256 * (B + 256 * A)).

itob(Int) ->
    D = Int rem 256,
    C = (Int div 256) rem 256,
    B = (Int div 256 div 256) rem 256,
    A = (Int div 256 div 256 div 256) rem 256,
    <<A, B, C, D>>.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

decode_handshake_test() ->
    InfoHash = crypto:rand_bytes(20),
    PeerId = crypto:rand_bytes(20),
    Handshake = handshake(InfoHash, PeerId),
    ?assertEqual({{handshake, InfoHash, PeerId}, <<>>}, decode(Handshake)).
decode_keep_alive_test() ->
    ?assertEqual({keep_alive, <<>>}, decode(keep_alive())).
decode_choke_test() ->
    ?assertEqual({choke, <<>>}, decode(<<0,0,0,1,0>>)).
decode_unchoke_test() ->
    ?assertEqual({unchoke, <<"foobar">>}, decode(<<0,0,0,1,1,"foobar">>)).

encode_request_test() ->
    ?assertEqual({{request, 1, 2, 3}, <<>>}, decode(request(1,2,3))).

itob_test() ->
    ?assertEqual(<<1,2,3,4>>, itob(16909060)).
