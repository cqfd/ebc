-module(ebc_bitfield).
-include_lib("eunit/include/eunit.hrl").

-export([at/2,
         add/2,
         contains/2]).

at(Index, Bitfield) ->
    <<_L:Index/bitstring, Bit:1, _R/bitstring>> = Bitfield,
    Bit.

add(Index, Bitfield) ->
    <<Left:Index/bitstring, _:1, Right/bitstring>> = Bitfield,
    <<Left/bitstring, 1:1, Right/bitstring>>.

contains(Index, Bitfield) ->
    at(Index, Bitfield) == 1.

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

contains_test() ->
    ?assert(contains(0, <<128>>)),
    ?assertNot(contains(1, <<128>>)),
    ?assert(contains(1, <<64>>)),
    ?assertNot(contains(0, <<64>>)),
    ?assert(contains(8, <<0, 128>>)),
    ?assertNot(contains(9, <<0, 128>>)).

difference_test() ->
    ?assertEqual([7], difference(<<1>>, <<0>>)),
    ?assertEqual([], difference(<<0>>, <<1>>)),
    ?assertEqual([], difference(<<1,2,3>>, <<1,2,3>>)).
