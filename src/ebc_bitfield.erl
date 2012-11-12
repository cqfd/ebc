-module(ebc_bitfield).
-include_lib("eunit/include/eunit.hrl").

-export([add/2,
         difference/2]).

add(Index, Bitfield) ->
    <<Left:Index/bitstring, _:1, Right/bitstring>> = Bitfield,
    <<Left/bitstring, 1:1, Right/bitstring>>.

difference(As, Bs) ->
    difference(As, Bs, 0, []).
difference(<<>>, <<>>, _Idx, Indices) ->
    lists:reverse(Indices);
difference(<<1:1, RestAs/bitstring>>,
           <<0:1, RestBs/bitstring>>,
           Idx, Indices) ->
    difference(RestAs, RestBs, Idx + 1, [Idx|Indices]);
difference(<<_:1, RestAs/bitstring>>,
           <<_:1, RestBs/bitstring>>,
           Idx, Indices) ->
    difference(RestAs, RestBs, Idx + 1, Indices).


%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

add_test() ->
    ?assertEqual(<<128>>, add(0, <<0>>)),
    ?assertEqual(<<64>>, add(1, <<0>>)),
    ?assertEqual(<<0, 128>>, add(8, <<0, 0>>)).

difference_test() ->
    ?assertEqual([7], difference(<<1>>, <<0>>)),
    ?assertEqual([], difference(<<0>>, <<1>>)),
    ?assertEqual([], difference(<<1,2,3>>, <<1,2,3>>)).
