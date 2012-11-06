-module(ebc_bitfield).
-include_lib("eunit/include/eunit.hrl").

-export([add/2]).

add(Index, Bitfield) when Index >= byte_size(Bitfield) * 8 ->
    MiddleSize = Index - byte_size(Bitfield),
    <<Bitfield/bitstring, 0:MiddleSize, 1:1>>;
add(0, Bitfield) ->
    <<_:1, Right/bitstring>> = Bitfield,
    <<1:1, Right/bitstring>>;
add(Index, Bitfield) ->
    <<Left:Index/bitstring, _:1, Right/bitstring>> = Bitfield,
    <<Left/bitstring, 1:1, Right/bitstring>>.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

add_test() ->
    ?assertEqual(<<1:1>>, add(0, <<0:1>>)),
    ?assertEqual(<<128>>, add(0, <<0>>)),
    ?assertEqual(<<255>>, add(7, <<254>>)),
    ?assertEqual(<<0, 1>>, add(15, <<>>)).
