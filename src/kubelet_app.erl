%%%-------------------------------------------------------------------
%% @doc etcd public API
%% @end
%%%-------------------------------------------------------------------

-module(kubelet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kubelet_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
