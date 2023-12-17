%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 14. 12月 2023 20:00
%%%-------------------------------------------------------------------
-author("Jeson").

-ifndef(USER_H).
-define(USER_H, true).

-record(user_online, {
    user_id = 0,
    username,
    net_pid,
    pid
}).

-record(user_robot, {
    username,
    user_id,
    pid
}).


-endif.