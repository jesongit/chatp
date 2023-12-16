%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 14. 12月 2023 15:15
%%%-------------------------------------------------------------------
-module(user_handle).
-author("Jeson").

-export([handle/1, test/1]).

-include("common.hrl").

handle(#user_login_request{username = Username, password = Password}) ->
    {ok, UserId, Pid} = user_mgr:login(self(), Username, Password),
    ?INFO("UserId ~p Pid ~p NetPid ~p Username ~s~n", [UserId, Pid, self(), Username]),
    #user_login_response{user_id = UserId};
handle(_Request) ->
    ?ERROR("Request ~w ~n", [_Request]),
    throw(unknown_protocol).

test(_) ->
    skip.
