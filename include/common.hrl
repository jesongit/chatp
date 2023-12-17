%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 12. 12月 2023 14:55
%%%-------------------------------------------------------------------
-author("Jeson").

-ifndef(COMMON_H).
-define(COMMON_H,1).

-include("protocol_pb.hrl").

-define(DEBUG(Format, Param),                   lager:debug("~p:~p:~p " ++ Format, [?MODULE, ?FUNCTION_NAME, ?LINE | Param])).
-define(DEBUG(String),                          ?DEBUG(String, [])).

-define(INFO(Format, Param),                    lager:info("~p:~p:~p " ++ Format, [?MODULE, ?FUNCTION_NAME, ?LINE | Param])).
-define(INFO(String),                           ?INFO(String, [])).

-define(ERROR(Format, Param),                   lager:error("~p:~p:~p " ++ Format, [?MODULE, ?FUNCTION_NAME, ?LINE | Param])).
-define(ERROR(String),                          ?ERROR(String, [])).

-define(PR_CATCH(Class, Reason, Stacktrace),    ?ERROR("~nStacktrace: ~s", [lager:pr_stacktrace(Stacktrace, {Class, Reason})])).

-define(CATCH(Fun, Default),                    try Fun catch C:R:S -> ?PR_CATCH(C, R, S), Default end).
-define(CATCH(Fun),                             ?CATCH(Fun, error)).

-define(IF(X, Y, Z),                            case X of true -> Y; false -> Z end).
-define(DEFAULT(X, Y),
    begin
        put('@return', X),
        case get('@return') of
            undefined ->
                Y;
            _ ->
                erase('@return')
        end
    end).

%%-define(ETS_LOOKUP(Tab, Key, Def),              case ets:lookup(Tab, Key) of [] -> Def; Ret -> Ret end).


-define(APP_LIST,                               [lager, ranch, gun]).
-define(MODULE_LIST,                            [id_utils, user_mgr, robot_mgr]).
-define(SUPERVISOR_LIST,                        [user_sup, robot_sup]).

%% id 分类
-define(ID_TYPE_USER,                           1).

-endif.