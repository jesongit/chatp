%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 14. 12月 2023 10:22
%%%-------------------------------------------------------------------
-module(tools).
-author("Jeson").

-compile(export_all).
-compile(nowarn_export_all).

term_to_binary(Binary) when is_binary(Binary) ->
    Binary;
term_to_binary(Term) when is_list(Term) orelse is_tuple(Term) orelse is_map(Term) ->
    list_to_binary(io_lib:format("~w", [Term])).

test(_) ->
    skip.
