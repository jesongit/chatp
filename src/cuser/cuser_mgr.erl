%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 13. 12月 2023 17:39
%%%-------------------------------------------------------------------
-module(cuser_mgr).
-author("Jeson").

-behaviour(gen_server).

%% API
-export([start_link/0, test/1, login/2, pid/1, register/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("cuser.hrl").
-include("common.hrl").
-include("cache_name.hrl").

-define(SERVER, ?MODULE).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

login(Username, Password) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, self(), Username, Password}).

register(Username, Password) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, self(), Username, Password}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?INFO("[~p] 启动完成", [?MODULE]),
    cache:new(?CACHE_USER, set, #user.user_id),
    ets:new(?ETS_ONLINE, [named_table, {keypos, #user_online.user_id}]),
    {ok, #state{}}.

handle_call({login, NetPid, Username, Password}, _From, State = #state{}) ->
    {reply, ?CATCH(do_login(NetPid, Username, Password)), State};
handle_call({register, NetPid, Username, Password}, _From, State = #state{}) ->
    {reply, ?CATCH(do_register(NetPid, Username, Password)), State};
handle_call(_Request, _From, State = #state{}) ->
    ?INFO("~p Request ~p From ~p~n", [?FUNCTION_NAME, _Request, _From]),
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    ?INFO("~p Request ~p~n", [?FUNCTION_NAME, _Request]),
    {noreply, State}.

handle_info(_Info, State = #state{}) ->
    ?INFO("~p Info ~p~n", [?FUNCTION_NAME, _Info]),
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ?INFO("~p Reason ~p~n", [?FUNCTION_NAME, _Reason]),
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    ?INFO("~p OldVsn ~p Extra ~p~n", [?FUNCTION_NAME, _OldVsn, _Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_login(NetPid, Username, Password) ->
    #user{password = Pass, user_id = UserId} = user(Username),
    Pass /= Password andalso throw(password),
    ?LOOKUP(?ETS_ONLINE, UserId) /= undefined andalso throw(online),
    {ok, Pid} = cuser_sup:start_child(UserId),
    ets:insert(?ETS_ONLINE, #user_online{user_id = UserId, pid = Pid, net_pid = NetPid, username = Username}),
    {ok, UserId}.

do_register(NetPid, Username, Password) ->
    user(Username) /= undefined andalso throw(existed),
    UserId = id_utils:gen_id(?ID_TYPE_USER),
    cache:insert(?CACHE_USER, #user{user_id = UserId, username = Username, password = Password}),
    do_login(NetPid, Username, Password).

pid(UserId) ->
    Online = ?LOOKUP(?ETS_ONLINE, UserId),
    Online == undefined andalso throw(not_online),
    Online#user_online.pid.

user(UserId) when is_integer(UserId) ->
    cache:lookup(?CACHE_USER, UserId);
user(Username) when is_binary(Username) ->
    cache:match(?CACHE_USER, #user{username = Username}).

test(_) ->
    skip.