-module(ebc_tracker).
-include_lib("eunit/include/eunit.hrl").

-export([start/2]).

start(Metainfo, PeerId) ->
    Params = [{"event", "started"},
              {"info_hash", bin_to_hex(ebc_minfo:info_hash(Metainfo))},
              {"peer_id", bin_to_hex(PeerId)},
              {"port", "6881"},
              {"uploaded", "0"},
              {"downloaded", "0"},
              {"left", ebc_minfo:width(Metainfo)}],
    Querys = [K ++ "=" ++ to_string(V) || {K, V} <- Params],
    QueryString = string:join(Querys, "&"),
    FullUrl = ebc_minfo:announce(Metainfo) ++ "?" ++ QueryString,
    {ok, {_S, _Hs, Body}} = httpc:request(get,
                                          {FullUrl, []},
                                          [],
                                          [{body_format, binary}]),
    Body.

bin_to_hex(Bin) ->
    lists:flatten([io_lib:format("%~2.16.0b", [B]) || <<B>> <= Bin]).

to_string(X) when is_list(X) ->
    X;
to_string(X) ->
    lists:flatten(io_lib:format("~p", [X])).

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------

start_test() ->
    {ok, Metainfo} = file:read_file("./test/test.torrent"),
    TInfo = start(Metainfo, crypto:rand_bytes(20)),
    ?assertEqual(1, ebc_tinfo:complete(TInfo)).
