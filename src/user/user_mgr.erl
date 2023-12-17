%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 13. 12月 2023 17:39
%%%-------------------------------------------------------------------
-module(user_mgr).
-author("Jeson").

-behaviour(gen_server).

%% API
-export([start_link/0, test/1, login/3, pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("user.hrl").
-include("common.hrl").
-include("ets_name.hrl").

-define(SERVER, ?MODULE).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

login(NetPid, Username, Password) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, NetPid, Username, Password}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?INFO("[~p] 启动完成", [?MODULE]),
    ets:new(?ETS_ONLINE, [set, public, named_table, {keypos, #user_online.user_id}]),
    {ok, #state{}}.

handle_call({login, NetPid, Username, _Password}, _From, State = #state{}) ->
    UserId = id_utils:gen_id(?ID_TYPE_USER),
    {ok, Pid} = user_sup:start_child(UserId),
    ets:insert(?ETS_ONLINE, #user_online{
        user_id = UserId, pid = Pid, net_pid = NetPid, username = Username}),
    {reply, {ok, UserId, Pid}, State};
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

pid(UserId) ->
    case ets:lookup(?ETS_ONLINE, UserId) of
        [] ->
            undefined;
        [#user_online{pid = Pid}] ->
            Pid
    end.

test(_) ->
    skip.