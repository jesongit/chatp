%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 12月 2023 15:23
%%%-------------------------------------------------------------------
-module(robot_handle).
-author("Jeson").

%% API
-export([
    response/1
]).

-include("user.hrl").
-include("common.hrl").
-include("ets_name.hrl").

response(#keep_alive_response{}) ->
    %% 保活协议
    ok;
response(#user_login_response{user_id = UserId, username = Username}) ->
    %% 登录成功
    ets:update_element(?ETS_ROBOT, Username, {#user_robot.user_id, UserId});
response(_Response) ->
    ?ERROR("Unknown Response ~w ~n", [_Response]).