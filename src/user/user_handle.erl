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

-export([ws_request/2, test/1, user_request/2]).

-include("user.hrl").
-include("common.hrl").
-include("ets_name.hrl").

ws_request(_, #user_login_request{username = Username, password = Password}) ->
    case ets:match(?ETS_ONLINE, #user_online{username = Username}) of
        [#user_online{user_id = UserId, pid = Pid}] ->
            ?INFO("Reconnect UserId ~p Pid ~p NetPid ~p Useranme ~s ~n", [UserId, Pid, self(), Username]),
            ets:update_element(?ETS_ONLINE, UserId, {#user_online.net_pid, self()});
        [] ->
            {ok, UserId, Pid} = user_mgr:login(self(), Username, Password),
            ?INFO("Login UserId ~p Pid ~p NetPid ~p Username ~s~n", [UserId, Pid, self(), Username]),
            ets:insert(?ETS_ONLINE, #user_online{username = Username, user_id = UserId, pid = Pid, net_pid = self()})
    end,
    #user_login_response{user_id = UserId, username = Username};
ws_request(UserId, Request) ->
    {ok, Response} = user:request(user_mgr:pid(UserId), Request),
    ?ERROR("Request ~w~nResponse~w~n", [Request, Response]),
    Response.

user_request(_UserId, _Request) ->
    ?ERROR("UserId ~p Request ~w ~n", [_UserId, _Request]).

test(_) ->
    skip.
