%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 15. 12月 2023 9:53
%%%-------------------------------------------------------------------
-module(id_utils).
-author("Jeson").

-behaviour(gen_server).

%% API
-export([start_link/0, test/1, gen_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("common.hrl").

-record(state, {id_map, svr_id}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

gen_id(Type) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Type}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?INFO("[~p] 启动完成", [?MODULE]),
    {ok, SvrId} = application:get_env(chat_p, server_id),
    {ok, #state{id_map = #{}, svr_id = SvrId}}.

handle_call({gen_id, Type}, _From, State = #state{svr_id = SvrId, id_map = Old}) ->
    {NewId, Index} = gen_new_id(SvrId, maps:get(Type, Old, 0)),
    ?DEBUG("Type ~p NewId ~p Index ~p ~n", [Type, NewId, Index]),
    {reply, NewId, State#state{id_map = Old#{Type => Index}}};
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
-define(SERVER_SHIFT_BITS,                          10).
-define(INDEX_SHIFT_BITS,                           22).
gen_new_id(SvrId, Index) ->
    {SvrId bsl ?INDEX_SHIFT_BITS + Index, Index + 1}.

test(_) ->
    skip.