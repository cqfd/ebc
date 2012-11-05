-module(ebc).
-include_lib("eunit/include/eunit.hrl").

-export([decode/1,
         decode_one/1,
         encode/1,
         bin_to_hex/1]).

decode(Bin) ->
    try
        {Result, _Rest} = decode_one(Bin),
        Result
    catch
        _Excpetion:_Reason ->
            error
    end.

decode_one(<<$i, S/binary>>) ->
    integer(S, []);
decode_one(<<$l, S/binary>>) ->
    list(S, []);
decode_one(<<$d, S/binary>>) ->
    dictionary(S, orddict:new());
decode_one(S) ->
    string(S, []).

encode([KvPair|Rest]) when is_tuple(KvPair) ->
    Encodings = << <<(encode(K))/binary, (encode(V))/binary>> || {K, V} <- [KvPair|Rest] >>,
    <<$d, Encodings/binary, $e>>;
encode(Xs) when is_list(Xs) ->
    Encodings = list_to_binary([encode(X) || X <- Xs]),
    <<$l, Encodings/binary, $e>>;
encode(X) when is_integer(X) ->
    <<$i, (list_to_binary(integer_to_list(X)))/binary, $e>>;
encode(Xs) when is_binary(Xs) ->
    Len = list_to_binary(integer_to_list(byte_size(Xs))),
    <<Len/binary, $:, Xs/binary>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

string(<<$:, Rest/binary>>, Digits) ->
    Len = list_to_integer(lists:reverse(Digits)),
    <<Result:Len/binary, Leftover/binary>> = Rest,
    {Result, Leftover};
string(<<Digit, Rest/binary>>, Digits) ->
    string(Rest, [Digit|Digits]).

integer(<<$e, Rest/binary>>, Acc) ->
    N = list_to_integer(lists:reverse(Acc)),
    {N, Rest};
integer(<<Digit, Rest/binary>>, Acc) ->
    integer(Rest, [Digit|Acc]).

list(<<$e, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
list(S, Acc) ->
    {X, Rest} = decode_one(S),
    list(Rest, [X|Acc]).

dictionary(<<$e, Rest/binary>>, Acc) ->
    {orddict:from_list(lists:reverse(Acc)), Rest};
dictionary(S, Acc) ->
    {Key, Rest} = decode_one(S),
    {Value, RestOfTheRest} = decode_one(Rest),
    dictionary(RestOfTheRest, [{Key, Value}|Acc]).

bin_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

decode_string_test() ->
    ?assertEqual(<<"bittorrent">>, decode(<<"10:bittorrent">>)).
decode_integer_test() ->
    ?assertEqual(1337, decode(<<"i1337e">>)).
decode_list_test() ->
    ?assertEqual([<<"foo">>, 123], decode(<<"l3:fooi123ee">>)).
decode_dict_test() ->
    ?assertEqual([{<<"cow">>, <<"moo">>}, {<<"spam">>, <<"eggs">>}],
                 decode(<<"d3:cow3:moo4:spam4:eggse">>)),
    ?assertEqual([{<<"spam">>, [<<"a">>, <<"b">>]}],
                 decode(<<"d4:spaml1:a1:bee">>)),
    ?assertEqual([{<<"a">>,
                  [{<<"b">>,
                    [{<<"c">>, <<"bittorrent">>}]}]}],
                 decode(<<"d1:ad1:bd1:c10:bittorrenteee">>)).

encode_string_test() ->
    ?assertEqual(<<"10:bittorrent">>, encode(<<"bittorrent">>)).
encode_integer_test() ->
    ?assertEqual(<<"i1337e">>, encode(1337)).
encode_list_test() ->
    ?assertEqual(<<"l3:foo3:bari1337ee">>, encode([<<"foo">>, <<"bar">>, 1337])).
encode_dict_test() ->
    Decoding = [{<<"a">>, <<"b">>},
                {<<"c">>, [<<"d">>,<<"e">>,<<"f">>]}],
    ?assertEqual(<<"d1:a1:b1:cl1:d1:e1:fee">>, encode(Decoding)).
