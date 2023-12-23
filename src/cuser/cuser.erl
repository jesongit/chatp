%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 13. 12月 2023 17:23
%%%-------------------------------------------------------------------
-module(cuser).
-author("Jeson").

-behaviour(gen_server).

%% API
-export([start_link/1, test/1, request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("cuser.hrl").
-include("common.hrl").
-include("cache_name.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).

request(Pid, Request) ->
    gen_server:call(Pid, {?FUNCTION_NAME, Request}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([UserId]) ->
    ?INFO("[~p] UserId ~p 启动完成", [?MODULE, UserId]),
    {ok, #user_state{user_id = UserId}}.

handle_call({request, Request}, _From, State = #user_state{}) ->
    try
        [_, Mod | _] = string:split(atom_to_list(element(1, Request)), "_", all),
        case erlang:apply(list_to_atom(Mod), request, [State, Request]) of
            ok ->
                {reply, #keep_alive_response{}, State};
            {ok, NewState} ->
                {reply, #keep_alive_response{}, NewState};
            {ok, Reply, NewState} ->
                {reply, Reply, NewState}
        end
    catch Class:Reason:Stacktrace ->
        ?PR_CATCH(Class, Reason, Stacktrace),
        {reply, #protocol{body = tools:term_to_binary(Reason)}}
    end;
handle_call(_Request, _From, State = #user_state{}) ->
    ?INFO("~p Request ~p From ~p~n", [?FUNCTION_NAME, _Request, _From]),
    {reply, ok, State}.

handle_cast(_Request, State = #user_state{}) ->
    ?INFO("~p Request ~p~n", [?FUNCTION_NAME, _Request]),
    {noreply, State}.

handle_info(_Info, State = #user_state{}) ->
    ?INFO("~p Info ~p~n", [?FUNCTION_NAME, _Info]),
    {noreply, State}.

terminate(_Reason, _State = #user_state{}) ->
    ?INFO("~p Reason ~p~n", [?FUNCTION_NAME, _Reason]),
    ok.

code_change(_OldVsn, State = #user_state{}, _Extra) ->
    ?INFO("~p OldVsn ~p Extra ~p~n", [?FUNCTION_NAME, _OldVsn, _Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

test(_) ->
    skip.