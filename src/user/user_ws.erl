%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 13. 12月 2023 10:51
%%%-------------------------------------------------------------------
-module(user_ws).
-author("Jeson").

-export([test/1, start/0, init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-include("common.hrl").

-define(IDLE_TIMEOUT,                                       60000). % 默认超时时间
-record(state,                                              {user_id}).

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/ws", ?MODULE, []}]}
    ]),
    {ok, Port} = application:get_env(chat_p, port),
    ?INFO("WebSocket Port ~p ~n", [Port]),
    {ok, Pid} = cowboy:start_clear(my_http_listener,
        [{port, Port}], #{env => #{dispatch => Dispatch}}
    ),
    {ok, Pid}.

init(Req, []) ->
    {cowboy_websocket, Req, #state{}, #{idle_timeout => ?IDLE_TIMEOUT}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc ws 连接上的回调
websocket_init(State) ->
    ?INFO("State ~w ~n", [State]),
    Response = handle_protocol(keep_alive, <<>>),
    {reply, {binary, protocol_pb:encode_msg(Response)}, State}.

%% @doc 接受 ws 消息
websocket_handle({binary, Binary}, State) ->
    ?DEBUG("Binary ~w ~n", [Binary]),
    case catch protocol_pb:decode_msg(Binary, protocol_request) of
        #protocol_request{cmd = Cmd, body = Body} ->
            Response = handle_protocol(Cmd, Body);
        _Ret ->
            ?ERROR("parse protocol error Ret ~w ~n", [Binary, _Ret]),
            Response = #protocol_response{cmd = unknown, code = error, body = <<"协议解密失败"/utf8>>}
    end,
    {reply, {binary, protocol_pb:encode_msg(Response)}, State};
websocket_handle(_Frame, State) ->
    ?INFO("Frame ~p ~n", [_Frame]),
    {ok, State}.

%% @doc 接受 erlang 消息
%%websocket_info({log, Text}, State) ->
%%    %% 返回一帧消息
%%    {reply, {text, Text}, State};
%%websocket_info(_Info, State) ->
%%    %% 返回多帧消息
%%    {reply, [
%%        {text, "Hello"},
%%        {text, <<"world!">>},
%%        {binary, <<0:8000>>}
%%    ], State};
%%websocket_info(_Info, State) ->
%%    %% 断开连接
%%    {stop, State}.
%%websocket_info(_Info, State) ->
%%    %% 发送关闭消息 {close, CloseCode, CloseReason}
%%    {reply, {close, 1000, <<"some-reason">>}, State}.
websocket_info(_Info, State) ->
    ?INFO("Info ~p ~n", [_Info]),
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ?INFO("Reason ~p Req ~p State ~p ~n", [_Reason, _Req, _State]),
    ok.

handle_protocol(keep_alive, _) ->
    #protocol_response{cmd = keep_alive, code = ok, body = protocol_pb:encode_msg(#keep_alive_response{})};
handle_protocol(Cmd, Body) ->
    ?DEBUG("Cmd ~p Body ~p ~n", [Cmd, Body]),
    try
        Request = list_to_atom(atom_to_list(Cmd) ++ "_request"),
        Return = user_handle:handle(protocol_pb:decode_msg(Body, Request)),
        ?DEBUG("Cmd ~p Response ~w ~n", [Cmd, Return]),
        #protocol_response{cmd = Cmd, code = ok, body = protocol_pb:encode_msg(Return)}
    catch Class:Reason:Stacktrace ->
        ?PR_CATCH(Class, Reason, Stacktrace),
        #protocol_response{cmd = Cmd, code = error, body = tools:term_to_binary(Reason)}
    end.

test(_) ->
    skip.
