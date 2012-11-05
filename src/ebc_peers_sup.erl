-module(ebc_peers_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Conn) ->
    supervisor:start_child(?SERVER, [Conn]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    PeerSup = {ebc_peer_sup, {ebc_peer_sup, start_link, []},
               transient, infinity, supervisor, [ebc_peer_sup]},
    {ok, { {simple_one_for_one, 0, 1}, [PeerSup]} }.
