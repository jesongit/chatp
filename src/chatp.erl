%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 12月 2023 23:11
%%%-------------------------------------------------------------------
-module(chatp).
-author("Jeson").

%% API
-export([start/0, stop/0]).

-include("common.hrl").

start() ->
    [ok = start_app(App) || App <- ?APP_LIST].

stop() ->
    init:stop().

start_app(App) ->
    case application:start(App) of
        ok ->
            ?INFO("App ~p start successful.~n", [App]);
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, Dep}} ->
            ok = start_app(Dep),
            start_app(App)
    end.
