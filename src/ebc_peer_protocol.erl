-module(ebc_peer_protocol).
-include_lib("eunit/include/eunit.hrl").
-include("ebc.hrl").

-export([decode_many/1,
         decode/1,
         encode/1]).

-spec decode_many(binary()) -> {[bittorrent_msg()], Rest :: binary()}.
decode_many(Bin) ->
    ebc:unfold(fun decode/1, Bin).

-spec decode(binary()) -> {bittorrent_msg(), Rest :: binary()}.
decode(<<19, "BitTorrent protocol",
         Reserved:8/bytes, InfoHash:20/bytes, PeerId:20/bytes,
         Rest/bytes>>) ->
    {{handshake, Reserved, InfoHash, PeerId}, Rest};
decode(<<0:32, Rest/binary>>) ->
    {keep_alive, Rest};
decode(<<Len:4/big-unsigned-integer-unit:8, Decoding:Len/bytes, Rest/bytes>>) ->
    {decode_by_id(Decoding), Rest};
decode(_Bin) ->
    nothing.

% Note that decode_by_id is designed to *crash* or malformed
% messages.
-spec decode_by_id(binary()) -> bittorrent_msg().
decode_by_id(<<0>>) ->
    choke;
decode_by_id(<<1>>) ->
    unchoke;
decode_by_id(<<2>>) ->
    interested;
decode_by_id(<<3>>) ->
    not_interested;
decode_by_id(<<4, Index:4/big-unsigned-integer-unit:8>>) ->
    {have, Index};
decode_by_id(<<5, Bitfield/bytes>>) ->
    {bitfield, Bitfield};
decode_by_id(<<6,
               Index:4/big-unsigned-integer-unit:8,
               Begin:4/big-unsigned-integer-unit:8,
               Length:4/big-unsigned-integer-unit:8>>) ->
    {request, Index, Begin, Length};
decode_by_id(<<7,
               Index:4/big-unsigned-integer-unit:8,
               Begin:4/big-unsigned-integer-unit:8,
               Block/bytes>>) ->
    {piece, Index, Begin, Block};
decode_by_id(<<8,
               Index:4/big-unsigned-integer-unit:8,
               Begin:4/big-unsigned-integer-unit:8,
               Length:4/big-unsigned-integer-unit:8>>) ->
    {cancel, Index, Begin, Length}.

-spec encode(bittorrent_msg()) -> binary().
encode({handshake, _Reserved, InfoHash, PeerId}) ->
    <<19, "BitTorrent protocol", 0:64, InfoHash/bytes, PeerId/bytes>>;
encode(keep_alive) ->
    <<0,0,0,0>>;
encode(choke) ->
    <<0,0,0,1,0>>;
encode(unchoke) ->
    <<0,0,0,1,1>>;
encode(interested) ->
    <<0,0,0,1,2>>;
encode(not_interested) ->
    <<0,0,0,1,3>>;
encode({have, PieceIndex}) ->
    <<0,0,0,5,4, PieceIndex:4/big-unsigned-integer-unit:8>>;
encode({bitfield, Bitfield}) ->
    <<(byte_size(Bitfield)):4/big-unsigned-integer-unit:8, 5, Bitfield/bytes>>;
encode({request, Index, Begin, Length}) ->
    <<0,0,0,13,6,
      Index:4/big-unsigned-integer-unit:8,
      Begin:4/big-unsigned-integer-unit:8,
      Length:4/big-unsigned-integer-unit:8>>;
encode({piece, Index, Begin, Block}) ->
    <<0,0,0,9,7,
      Index:4/big-unsigned-integer-unit:8,
      Begin:4/big-unsigned-integer-unit:8,
      Block/bytes>>;
encode({cancel, Index, Begin, Length}) ->
    <<0,0,0,13,8,
      Index:4/big-unsigned-integer-unit:8,
      Begin:4/big-unsigned-integer-unit:8,
      Length:4/big-unsigned-integer-unit:8>>.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

decode_handshake_test() ->
    InfoHash = crypto:rand_bytes(20),
    PeerId = crypto:rand_bytes(20),
    Handshake = encode({handshake, <<0:64>>, InfoHash, PeerId}),
    ?assertEqual({{handshake, <<0:64>>, InfoHash, PeerId}, <<>>}, decode(Handshake)).
decode_keep_alive_test() ->
    ?assertEqual({keep_alive, <<>>}, decode(encode(keep_alive))).
decode_choke_test() ->
    ?assertEqual({choke, <<>>}, decode(<<0,0,0,1,0>>)).
decode_unchoke_test() ->
    ?assertEqual({unchoke, <<"foobar">>}, decode(<<0,0,0,1,1,"foobar">>)).

decode_partial_have_msg_test() ->
    PartialHaveMsg = <<0,0,0,5,4,1,2,3>>,
    ?assertEqual(nothing, decode(PartialHaveMsg)).

decode_junk_test() ->
    Junk = <<0,0,0,1,19>>,
    ?assertError(function_clause, decode(Junk)).

decode_many_test() ->
    KeepAlive = <<0,0,0,0>>,
    Have = <<0,0,0,5,4,0,0,0,1>>,
    Partial = <<0,0,0,1>>,
    Encoding = <<KeepAlive/bytes, Have/bytes, Partial/bytes>>,
    ?assertEqual({[keep_alive, {have, 1}], Partial}, decode_many(Encoding)).

encode_request_test() ->
    ?assertEqual({{request, 1, 2, 3}, <<>>}, decode(encode({request,1,2,3}))).
