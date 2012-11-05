-module(ebc_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = {one_for_all, 0, 1},
    PeerAcceptor = {ebc_peer_acceptor, {ebc_peer_acceptor, start_link, []},
                    permanent, 5000, worker, [ebc_peer_acceptor]},
    PeersSup = {ebc_peers_sup, {ebc_peers_sup, start_link, []},
                permanent, infinity, supervisor, [ebc_peers_sup]},
    {ok, {RestartStrategy, [PeersSup, PeerAcceptor]}}.
