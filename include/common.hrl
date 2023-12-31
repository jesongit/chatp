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

-define(DEBUG(Format, Param),                   lager:debug(Format, Param)).
-define(DEBUG(String),                          ?DEBUG(String, [])).

-define(INFO(Format, Param),                    lager:info(Format, Param)).
-define(INFO(String),                           ?INFO(String, [])).

-define(ERROR(Format, Param),                   lager:error(Format, Param)).
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

-define(LOOKUP(Tab, Key),                           tools:ets_lookup(Tab, Key)).
-define(INSERT(Tab, Data),                          tools:ets_insert(Tab, Data)).
-define(MATCH(Tab, Pattern),                        tools:ets_match(Tab, Pattern)).

-define(INIT_LIST,                                  [global_data]).
-define(APP_LIST,                                   [ranch, gun, chatp]).
-define(MODULE_LIST,                                [reloader, id_utils, cuser_mgr, robot_mgr]).
-define(SUPERVISOR_LIST,                            [cuser_sup, robot_sup]).

%% id 分类
-define(ID_TYPE_USER,                           1).

-endif.