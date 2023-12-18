%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 18. 12月 2023 17:38
%%%-------------------------------------------------------------------
-module(cache).
-author("Jeson").

-export([test/1, new/3, lookup/2, insert/2, match/2]).

-define(DETS_DIR(File),                         "./dets/" ++ File ++ ".dets").
new(Name, Type, KeyPos) ->
    {ok, Name} = dets:open_file(Name, [{type, Type}, {keypos, KeyPos}, {file, ?DETS_DIR(Name)}]),
    ets:new(Name, [named_table, Type, {keypos, KeyPos}]).

lookup(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] ->
            case dets:lookup(Tab, Key) of
                {error, _Reason} ->
                    undefined;
                Object ->
                    ets:insert(Tab, Object),
                    Object
            end;
        [Object] ->
            Object
    end.

insert(Tab, Data) ->
    ok = dets:insert(Tab, Data),
    true = ets:insert(Tab, Data),
    Data.

match(Tab, Pattern) ->
    case ets:match_object(Tab, Pattern) of
        [] ->
            case dets:match_object(Tab, Pattern) of
                [] ->
                    [];
                Objects when is_list(Objects) ->
                    Objects
            end;
        Objects when is_list(Objects) ->
            Objects
    end.

test(_) ->
    skip.
