-module(etwit_sup).

-include("etwit.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SERVER = {
      ?TWIT_SERVER,
      {etwit_server, start_link, [?TWIT_SERVER]},
      permanent,
      1500,
      worker,
      [etwit_server]
     },
    {ok, {{one_for_one, 5, 10}, [SERVER]}}.

