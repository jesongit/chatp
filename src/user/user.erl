%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 13. 12月 2023 17:23
%%%-------------------------------------------------------------------
-module(user).
-author("Jeson").

-behaviour(gen_server).

%% API
-export([start_link/1, test/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("common.hrl").

-record(state, {user_id}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([UserId]) ->
    ?INFO("[~p] UserId ~p 启动完成", [?MODULE, UserId]),
    {ok, #state{user_id = UserId}}.

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