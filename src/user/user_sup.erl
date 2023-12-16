%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 13. 12月 2023 17:54
%%%-------------------------------------------------------------------
-module(user_sup).
-author("Jeson").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(UserId) ->
    supervisor:start_child(?MODULE, [UserId]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
-define(USER_MOD,                                       user).
init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [{?USER_MOD, {?USER_MOD, start_link, []}, temporary, 5000, worker, [?USER_MOD]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
