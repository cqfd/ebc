-module(ebc).
-include("ebc.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([decode/1,
         encode/1,
         unfold/2]).

decode(Bin = <<$i, _/binary>>) ->
    integer(Bin);
decode(Bin = <<$l, _/binary>>) ->
    list(Bin);
decode(Bin = <<$d, _/binary>>) ->
    dictionary(Bin);
decode(Bin) ->
    string(Bin).

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

string(Bin) ->
    case num(Bin) of
        nothing ->
            nothing;
        {Len, <<$:, RestOne/bytes>>} ->
            case RestOne of
                <<Result:Len/bytes, RestTwo/bytes>> ->
                    {Result, RestTwo};
                _ ->
                    nothing
            end;
        _ ->
            nothing
    end.

integer(<<$i, $-, Bin/bytes>>) ->
    case num(Bin) of
        {Num, <<$e, Rest/bytes>>} ->
            {-Num, Rest};
        _ ->
            nothing
    end;
integer(<<$i, Bin/bytes>>) ->
    case num(Bin) of
        {Num, <<$e, Rest/bytes>>} ->
            {-Num, Rest};
        _ ->
            nothing
    end;
integer(_) ->
    nothing.

list(<<$l, Bin/bytes>>) ->
    case unfold(fun decode/1, Bin) of
        {Results, <<$e, Rest/bytes>>} ->
            {Results, Rest};
        _ ->
            nothing
    end;
list(_) ->
    nothing.

dictionary(<<$d, Bin/binary>>) ->
    case unfold(fun kv/1, Bin) of
        {KvPairs, <<$e, Rest/bytes>>} ->
            {KvPairs, Rest};
        _ ->
            nothing
    end.

num(Bin) ->
    case unfold(fun digit/1, Bin) of
        {[], Bin} ->
            nothing;
        {Digits, Rest} ->
            {list_to_integer(Digits), Rest}
    end.

digit(<<X, Rest/binary>>) when $0 =< X, X =< $9 ->
    {X, Rest};
digit(_Bin) ->
    nothing.

kv(Bin) ->
    case string(Bin) of
        nothing ->
            nothing;
        {Key, RestOne} ->
            case decode(RestOne) of
                nothing ->
                    nothing;
                {Value, RestTwo} ->
                    {{Key, Value}, RestTwo}
            end
    end.

unfold(F, X) ->
    unfold(F, X, []).
unfold(F, X, Acc) ->
    case F(X) of
        nothing ->
            {lists:reverse(Acc), X};
        {A, X2} ->
            unfold(F, X2, [A|Acc])
    end.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

num_test() ->
    ?assertEqual({1337, <<>>}, num(<<"1337">>)).

decode_string_test() ->
    ?assertEqual({<<"bittorrent">>, <<>>}, decode(<<"10:bittorrent">>)).
decode_integer_test() ->
    ?assertEqual({1337, <<>>}, decode(<<"i1337e">>)).
decode_list_test() ->
    ?assertEqual({[<<"foo">>, 123], <<>>}, decode(<<"l3:fooi123ee">>)).
decode_dict_test() ->
    ?assertEqual({[{<<"cow">>, <<"moo">>}, {<<"spam">>, <<"eggs">>}], <<>>},
                 decode(<<"d3:cow3:moo4:spam4:eggse">>)),
    ?assertEqual({[{<<"spam">>, [<<"a">>, <<"b">>]}], <<>>},
                 decode(<<"d4:spaml1:a1:bee">>)),
    ?assertEqual({[{<<"a">>,
                    [{<<"b">>,
                      [{<<"c">>, <<"bittorrent">>}]}]}], <<>>},
                 decode(<<"d1:ad1:bd1:c10:bittorrenteee">>)).

decode_junky_string_test() ->
    ?assertEqual(nothing, decode(<<"4x:eggs">>)).
decode_junky_int_test() ->
    ?assertEqual(nothing, decode(<<"i133xe">>)).
decode_junky_list_test() ->
    ?assertEqual(nothing, decode(<<"l3:foo3:bar3:bazxe">>)).
decode_junky_dict_test() ->
    ?assertEqual(nothing, decode(<<"d3:key5:valuexe">>)).

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

unfold_test() ->
    Xs = [1,2,3],
    Result = unfold(fun([]) ->
                            nothing;
                       ([X|Rest]) ->
                            {X, Rest}
                    end,
                    Xs),
    ?assertEqual({Xs, []}, Result).
