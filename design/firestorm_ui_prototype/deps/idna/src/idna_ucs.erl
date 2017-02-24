%%% -*- erlang -*-
%%%
%%% This file is part of erlang-idna released under the BSD license.
%%% See the LICENSE for more information.
%%%
%%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
%%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% import some function from the xmerl_ucs module to avoid the need to
%% include xmerl.
-module(idna_ucs).

-export([from_utf8/1]).
-export([is_ascii/1]).

%%% Test for legitimate ASCII code
is_ascii(Ch) when is_integer(Ch), Ch >= 0, Ch =< 127 -> true;
is_ascii(_) -> false.

from_utf8(Bin) when is_binary(Bin) -> from_utf8(binary_to_list(Bin));
from_utf8(List) ->
    case expand_utf8(List) of
        {Result,0} -> Result;
        {_Res,_NumBadChar} ->
            exit({ucs,{bad_utf8_character_code}})
    end.

%% expand_utf8([Byte]) -> {[UnicodeChar],NumberOfBadBytes}
%% Expand UTF8 byte sequences to ISO 10646/Unicode
%% charactes. Any illegal bytes are removed and the number of
%% bad bytes are returned.
%%
%% Reference:
%% RFC 3629: "UTF-8, a transformation format of ISO 10646".

expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
                                         C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
        C when 16#80 =< C ->
            expand_utf8_1(Cs, [C|Acc], Bad);
        _ ->
            %% Bad range.
            expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
                                            C2 band 16#C0 =:= 16#80,
                                            C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
        (C3 band 16#3F) of
        C when 16#800 =< C ->
            expand_utf8_1(Cs, [C|Acc], Bad);
        _ ->
            %% Bad range.
            expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
                                               C2 band 16#C0 =:= 16#80,
                                               C3 band 16#C0 =:= 16#80,
                                               C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
           (C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
        C when 16#10000 =< C ->
            expand_utf8_1(Cs, [C|Acc], Bad);
        _ ->
            %% Bad range.
            expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([_|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {lists:reverse(Acc),Bad}.
