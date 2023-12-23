%%%-------------------------------------------------------------------
%% @doc chatp public API
%% @end
%%%-------------------------------------------------------------------

-module(chatp_app).

-behaviour(application).

-export([start/2, stop/1, test/1]).

-include("common.hrl").

start(_StartType, _StartArgs) ->
    {ok, Pid} = chatp_sup:start_link(),
    [start_sup(Mod) || Mod <- ?SUPERVISOR_LIST],
    [start_mod(Mod) || Mod <- ?MODULE_LIST],
    [Mod:init() || Mod <- ?INIT_LIST],
    {ok, WsPid} = cuser_ws:start(),
    ?INFO("chatp pid ~p Cowboy ~p ~n", [Pid, WsPid]),
    {ok, Pid}.   % 这里一定要返回顶部监督者的pid，不然init:stop()无法正常关闭

stop(_State) ->
    ok.

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
