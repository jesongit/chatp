%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 18. 12月 2023 16:01
%%%-------------------------------------------------------------------
-module(login).
-author("Jeson").

-export([test/1, request/2]).

request(_User, _) ->
    erlang:error(not_implemented).

test(_) ->
    skip.
