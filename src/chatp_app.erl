%%%-------------------------------------------------------------------
%% @doc chatp public API
%% @end
%%%-------------------------------------------------------------------

-module(chatp_app).

-behaviour(application).

-export([start/2, stop/1, test/1, start/0, stop/0]).

-include("common.hrl").

start() ->
    ?INFO("chatp start."),
    application:start(chatp).

stop() ->
    ?INFO("chatp stop."),
    init:stop().

start(_StartType, _StartArgs) ->
    [ok = start_app(App) || App <- ?APP_LIST],
    {ok, _Pid} = chatp_sup:start_link(),
    [start_sup(Mod) || Mod <- ?SUPERVISOR_LIST],
    [start_mod(Mod) || Mod <- ?MODULE_LIST],
    [Mod:init() || Mod <- ?INIT_LIST],
    user_ws:start().

stop(_State) ->
    ok.

start_app(App) ->
    case application:start(App) of
        ok ->
            io:format("App ~p start successful.~n", [App]);
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, Dep}} ->
            ok = start_app(Dep),
            start_app(App)
    end.

start_mod(Mod) ->
    start_child(Mod, worker).

start_sup(Mod) ->
    start_child(Mod, supervisor).

start_child(Mod, Type) ->
    ChildSpec = {Mod, {Mod, start_link, []}, permanent, 5000, Type, [Mod]},
    {ok, Pid} = supervisor:start_child(chatp_sup, ChildSpec),
    ?INFO("[~p] Start ~p Pid ~p ~n", [Mod, Type, Pid]).

test(_) ->
    skip.
