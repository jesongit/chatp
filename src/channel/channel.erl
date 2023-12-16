%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 15. 12月 2023 10:47
%%%-------------------------------------------------------------------
-module(channel).
-author("Jeson").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([start_link/0]).

-define(SERVER, ?MODULE).

-record(channel_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #channel_state{}} | {ok, State :: #channel_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #channel_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #channel_state{}) ->
    {reply, Reply :: term(), NewState :: #channel_state{}} |
    {reply, Reply :: term(), NewState :: #channel_state{}, timeout() | hibernate} |
    {noreply, NewState :: #channel_state{}} |
    {noreply, NewState :: #channel_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #channel_state{}} |
    {stop, Reason :: term(), NewState :: #channel_state{}}).
handle_call(_Request, _From, State = #channel_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #channel_state{}) ->
    {noreply, NewState :: #channel_state{}} |
    {noreply, NewState :: #channel_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #channel_state{}}).
handle_cast(_Request, State = #channel_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #channel_state{}) ->
    {noreply, NewState :: #channel_state{}} |
    {noreply, NewState :: #channel_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #channel_state{}}).
handle_info(_Info, State = #channel_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #channel_state{}) -> term()).
terminate(_Reason, _State = #channel_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #channel_state{},
    Extra :: term()) ->
    {ok, NewState :: #channel_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #channel_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
