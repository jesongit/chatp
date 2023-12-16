%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 14. 12月 2023 18:06
%%%-------------------------------------------------------------------
-module(robot_mgr).
-author("Jeson").

-behaviour(gen_server).

%% API
-export([start_link/0, test/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("user.hrl").
-include("common.hrl").
-include("ets_name.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?INFO("[~p] 启动完成", [?MODULE]),
    ets:new(?ETS_ROBOT, [set, public, named_table, {keypos, #user_robot.user_id}]),
    {ok, #state{}}.

handle_call({start, Username}, _From, State = #state{}) ->
    case ets:match_object(?ETS_ONLINE, #user_online{username = Username, _ = '_'}) of
        [] ->
            case ets:match_object(?ETS_ROBOT, #user_robot{username = Username, _ = '_'}) of
                [] ->
                    {ok, Pid} = robot_sup:start_child(Username),
                    {reply, {ok, Pid}, State};
                _ ->
                    {reply, {error, already}, State}
            end;
        _ ->
            {reply, {error, online}, State}
    end;
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

test(_) ->
    skip.