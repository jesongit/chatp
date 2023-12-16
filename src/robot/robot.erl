%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(robot).

-behaviour(gen_server).

-export([start_link/1, start/1, stop/0, stop/1, send/3, pid/1, test/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("user.hrl").
-include("common.hrl").
-include("ets_name.hrl").

-define(SERVER,                             robot_mgr).
-define(SERVER_IP,                          "127.0.0.1").

-record(state, {username, pid, ref}).

start(Username) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Username}).

stop() ->
    gen_server:cast(?SERVER, {?FUNCTION_NAME}).

stop(Username) ->
    gen_server:cast(?SERVER, {?FUNCTION_NAME, Username}).

send(Username, Cmd, Body) ->
    gen_server:cast(pid(Username), {send, Cmd, Body}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Username) ->
    gen_server:start_link(?MODULE, [Username], []).

init([Username]) ->
    {ok, ConnPid, StreamRef} = connect_server(),
    ?INFO("User: ~s ConnPid ~p StremRef ~p ~n", [Username, ConnPid, StreamRef]),
    {ok, #state{username = Username, pid = ConnPid, ref = StreamRef}}.

handle_call(_Request, _From, State = #state{}) ->
    ?INFO("~p Request ~p From ~p~n", [?FUNCTION_NAME, _Request, _From]),
    {reply, ok, State}.

handle_cast({send, Cmd, Body}, State = #state{pid = Pid, ref = Ref}) ->
    ?DEBUG("Send Cmd ~p Body ~w ~n", [Cmd, Body]),
    Request = protocol_pb:encode_msg(#protocol_request{cmd = Cmd, body = protocol_pb:encode_msg(Body)}),
    gun:ws_send(Pid, Ref, [{binary, Request}]),
    {noreply, State};
handle_cast(_Request, State = #state{}) ->
    ?INFO("~p Request ~p~n", [?FUNCTION_NAME, _Request]),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State = #state{username = User, pid = Pid}) ->
    ?INFO("User ~s ConnPid ~p Stop Connect Reason ~p ~n", [User, Pid, Reason]),
    {stop, Reason, State};
handle_info({gun_up, ConnPid, http}, State = #state{username = User}) ->
    ?INFO("Pid ~p Connect Successful.~n", [ConnPid]),
    handle_cast({send, user_login, #user_login_request{username = User, password = User}}, State);
handle_info(keep_alive, State = #state{}) ->
    erlang:send_after(5000, self(), keep_alive),
    handle_cast({send, keep_alive, #keep_alive_request{}}, State);
handle_info({gun_down, ConnPid, http, Name, Reason}, State = #state{}) ->
    ?INFO("Pid ~p Name ~p Reason ~w ~n", [ConnPid, Name, Reason]),
    {noreply, State};
handle_info({gun_ws, _, _, Frame}, State = #state{username = User}) ->
    ?DEBUG("User ~s receive Frame ~p ~n", [User, Frame]),
    handle_frame(Frame),
    {noreply, State};
handle_info(_Info, State = #state{}) ->
    ?INFO("~p Info ~p~n", [?FUNCTION_NAME, _Info]),
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_response(#keep_alive_response{}) ->
    ?DEBUG("keep alive");
handle_response(_Protocol) ->
    ?ERROR("unknown protocol ~w ~n", [_Protocol]).

handle_frame({close, Code, Reason}) ->
    ?INFO("Close Code ~p Reason ~w~n", [Code, Reason]);
handle_frame({binary, Binary}) ->
    #protocol_response{cmd = Cmd, body = Body} = protocol_pb:decode_msg(Binary, protocol_response),
    Response = list_to_atom(atom_to_list(Cmd) ++ "_response"),
    handle_response(protocol_pb:decode_msg(Body, Response)).

connect_server() ->
    {ok, Port} = application:get_env(chat_p, port),
    {ok, ConnPid} = gun:open(?SERVER_IP, Port),
    _MRef = monitor(process, ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            {ok, ConnPid, StreamRef};
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after 1000 ->
        exit(timeout)
    end.

pid(UserId) when is_integer(UserId)->
    case ets:lookup(?ETS_ROBOT, UserId) of
        [#user_robot{pid = Pid}] ->
            Pid;
        [] ->
            not_found
    end;
pid(Username) ->
    case ets:match(?ETS_ROBOT, #user_robot{username = Username, pid = '$1', _ = '_'}) of
        [[Pid]] ->
            Pid;
        [] ->
            not_found
    end.

test(_) ->
    start(<<"1">>).