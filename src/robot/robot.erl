%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(robot).

-behaviour(gen_server).

-export([start_link/1, start/1, stop/0, stop/1, send/3, test/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("cuser.hrl").
-include("common.hrl").
-include("cache_name.hrl").

-define(SERVER,                             robot_mgr).
-define(SERVER_IP,                          "127.0.0.1").
-define(KEEP_ALIVE_INTERVAL,                5000).
-define(CHECK_ALIVE_SECONDS,                10).

-record(state, {username, pid, ref, alive_time = 0}).

start(Username) ->
    gen_server:call(?SERVER, {?FUNCTION_NAME, Username}).

stop() ->
    gen_server:cast(?SERVER, {?FUNCTION_NAME}).

stop(Username) ->
    gen_server:cast(?SERVER, {?FUNCTION_NAME, Username}).

send(Pid, Cmd, Body) when is_pid(Pid), is_atom(Cmd), is_tuple(Body) ->
    gen_server:cast(Pid, {send, Cmd, Body}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Username) ->
    gen_server:start_link(?MODULE, [Username], []).

init([Username]) ->
    {ok, ConnPid, StreamRef} = connect_server(),
    ?INFO("User: ~s ConnPid ~p StremRef ~p ~n", [Username, ConnPid, StreamRef]),
    erlang:send_after(?KEEP_ALIVE_INTERVAL * 2, self(), check_alive),
    {ok, #state{username = Username, pid = ConnPid, ref = StreamRef}}.

handle_call(_Request, _From, State = #state{}) ->
    ?INFO("~p Request ~p From ~p~n", [?FUNCTION_NAME, _Request, _From]),
    {reply, ok, State}.

handle_cast({send, Cmd, Body}, State = #state{pid = Pid, ref = Ref}) ->
    ?DEBUG("Send Cmd ~p Body ~w ~n", [Cmd, Body]),
    Request = protocol_pb:encode_msg(#protocol{cmd = Cmd, body = protocol_pb:encode_msg(Body)}),
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
    user_login(User, User),
    {noreply, State};
handle_info(keep_alive, State = #state{}) ->
    keep_alive(),
    {noreply, State#state{alive_time = tools:time()}};
handle_info(check_alive, State = #state{}) ->
    check_alive(State);
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

terminate(_Reason, _State = #state{pid = Pid}) ->
    gun:close(Pid),
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

user_login(Username, Password) ->
    gen_server:cast(self(), {send, user_login, #user_login_request{username = Username, password = Password}}).

keep_alive() ->
    gen_server:cast(self(), {send, keep_alive, #keep_alive_request{}}),
    erlang:send_after(?KEEP_ALIVE_INTERVAL, self(), keep_alive).

check_alive(State = #state{alive_time = AliveTime}) ->
    erlang:send_after(60000, self(), check_alive),
    ?IF(tools:time() - AliveTime > ?KEEP_ALIVE_INTERVAL div 1000 * 2, {stop, State}, {noreply, State}).

connect_server() ->
    {ok, Port} = application:get_env(chatp, port),
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

handle_frame({close, Code, Reason}) ->
    ?INFO("Close Code ~p Reason ~w~n", [Code, Reason]);
handle_frame({binary, Binary}) ->
    try
        #protocol{cmd = Cmd, body = Body} = protocol_pb:decode_msg(Binary, protocol),
        Cmd == unknown andalso throw(Body),
        Response = list_to_atom(atom_to_list(Cmd) ++ "_response"),
        robot_handle:response(protocol_pb:decode_msg(Body, Response))
    catch Class:Reason:Stacktrace ->
        ?PR_CATCH(Class, Reason, Stacktrace),
        Class == throw andalso is_binary(Reason) andalso ?DEBUG("Reason ~ts ~n", [Reason])
    end.

test(_) ->
    start(<<"1">>).