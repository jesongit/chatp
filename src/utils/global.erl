%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 18. 12月 2023 18:01
%%%-------------------------------------------------------------------
-module(global).
-author("Jeson").

-compile(export_all).
-compile(nowarn_export_all).

-include("cache_name.hrl").

init() ->
    cache:new(?CACHE_GLOBAL, set, 1).

get(Key) ->
    case cache:lookup(?CACHE_GLOBAL, Key) of
        undefined ->
            undefined;
        {Key, Val} ->
            Val
    end.

set(Key, Val) ->
    cache:insert(?CACHE_GLOBAL, {Key, Val}).
