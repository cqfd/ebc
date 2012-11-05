-module(ebc_peer_sup).
-behaviour(supervisor).

%% API
-export([start_link/1,
         start_reader/3,
         get_fsm/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Conn) ->
    supervisor:start_link(?MODULE, Conn).

start_reader(Sup, Conn, Fsm) ->
    Reader = {ebc_peer_reader, {ebc_peer_reader, start_link, [Conn, Fsm]},
              transient, 5000, worker, [ebc_peer_reader]},
    supervisor:start_child(Sup, Reader).

get_fsm(Sup) ->
    Children = supervisor:which_children(Sup),
    find_child(ebc_peer_fsm, Children).

%% Supervisor callbacks
%% ===================================================================

init(Conn) ->
    PeerFsm = {ebc_peer_fsm, {ebc_peer_fsm, start_link, [Conn]},
               transient, 5000, worker, [ebc_peer_fsm]},
    {ok, { {one_for_all, 0, 1}, [PeerFsm]} }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_child(_ChildId, []) ->
    error;
find_child(ChildId, [{OtherChildId, Pid, _Type, _Modules}|Children]) ->
    case ChildId =:= OtherChildId of
        true ->
            {ok, Pid};
        false ->
            find_child(ChildId, Children)
    end.
