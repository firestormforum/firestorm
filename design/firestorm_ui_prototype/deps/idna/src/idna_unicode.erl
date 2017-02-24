-module(idna_unicode).

-export([compose/1,
         decompose/1,
         downcase/1,
         normalize_kc/1,
         sort_canonical/1]).

%%============================================================================
%% Constants
%%============================================================================

-define(HANGUL_SBASE, 16#ac00).

-define(HANGUL_LBASE, 16#1100).

-define(HANGUL_LCOUNT, 19).

-define(HANGUL_VBASE, 16#1161).

-define(HANGUL_VCOUNT, 21).

-define(HANGUL_TBASE, 16#11a7).

-define(HANGUL_TCOUNT, 28).

-define(HANGUL_NCOUNT, 588). % ?HANGUL_VCOUNT * ?HANGUL_TCOUNT

-define(HANGUL_SCOUNT, 11172). % ?HANGUL_LCOUNT * ?HANGUL_NCOUNT

-define(COMBINING_CLASS, 1).
-define(DECOMPOSITION, 2).
-define(LOWERCASE_MAPPING, 3).

%%============================================================================
%% API
%%============================================================================

compose([]) ->
    [];
compose([Starter|Unicode]) ->
    StarterCC = case combining_class(Starter) of
                    0 -> 0;
                    _ -> 256
                end,
    compose(Starter, StarterCC, Unicode, []).

decompose(Unicode) ->
    lists:reverse(lists:foldl(fun(CP, Acc) ->
                                      codepoint_decompose(CP, Acc)
                              end, [], Unicode)).

downcase(Unicode) ->
    [codepoint_downcase(CP) || CP <- Unicode].

normalize_kc(Unicode) ->
    compose(sort_canonical(decompose(Unicode))).

sort_canonical(Unicode) ->
    Length = length(Unicode),
    case Length < 2 of
        true ->
            Unicode;
        false ->
            sort_canonical(array:from_list(Unicode), 1, Length)
    end.

%%============================================================================
%% Helper functions
%%============================================================================

compose(Starter, _, [], Acc) ->
    lists:reverse([Starter|Acc]);
compose(Starter, StarterCC, [CP|Unicode], Acc) ->
    Composite = compose_pair(Starter, CP),
    case StarterCC =:= 0 andalso Composite =/= undefined of
        true ->
            compose(Composite, combining_class(Composite),
                    Unicode, Acc);
        false ->
            compose(CP, combining_class(CP), Unicode,
                    [Starter|Acc])
    end.

compose_pair(A, B) when
      A >= ?HANGUL_LBASE andalso
      A < (?HANGUL_LBASE + ?HANGUL_LCOUNT) andalso
      B >= ?HANGUL_LBASE andalso
      B < (?HANGUL_VBASE + ?HANGUL_VCOUNT) ->
    ?HANGUL_SBASE + ((A - ?HANGUL_LBASE) * ?HANGUL_VCOUNT +
                     (B - ?HANGUL_VBASE)) * ?HANGUL_TCOUNT;
compose_pair(A, B) when
      A >= ?HANGUL_SBASE andalso
      A < (?HANGUL_SBASE + ?HANGUL_SCOUNT) andalso
      ((A - ?HANGUL_SBASE) rem ?HANGUL_TCOUNT) =:= 0 andalso
      B >= ?HANGUL_TBASE andalso
      B < (?HANGUL_TBASE + ?HANGUL_TCOUNT) ->
    A + (B - ?HANGUL_TBASE);
compose_pair(A, B) ->
    composition(A, B).

codepoint_decompose(CP, Acc) when
      CP >= ?HANGUL_SBASE andalso CP < (?HANGUL_SBASE + ?HANGUL_SCOUNT) ->
    lists:reverse(decompose_hangul(CP), Acc);
codepoint_decompose(CP, Acc) ->
    case compat(CP) of
        {error, bad_codepoint} ->
            [CP|Acc];
        undefined ->
            [CP|Acc];
        Codepoints ->
            lists:reverse(decompose(Codepoints), Acc)
    end.

decompose_hangul(CP) ->
    Sindex = CP - ?HANGUL_SBASE,
    case (Sindex < 0 orelse Sindex >= ?HANGUL_SCOUNT) of
        true ->
            [CP];
        false ->
            L = ?HANGUL_LBASE + Sindex div ?HANGUL_NCOUNT,
            V = ?HANGUL_VBASE + (Sindex rem ?HANGUL_NCOUNT) div ?HANGUL_TCOUNT,
            T = ?HANGUL_TBASE + Sindex rem ?HANGUL_TCOUNT,
            case T =:= ?HANGUL_TBASE of
                true ->
                    [L, V];
                false ->
                    [L, V, T]
            end
    end.

codepoint_downcase(CP) ->
    case lowercase(CP) of
        {error, bad_codepoint} ->
            CP;
        Lowercase ->
            Lowercase
    end.

sort_canonical(Unicode, I, Length) ->
    case I < Length of
        true ->
            array:to_list(Unicode);
        false ->
            Last = array:get(I - 1, Unicode),
            CP = array:get(I, Unicode),
            LastCC = combining_class(Last),
            CC = combining_class(CP),
            case CC =/= 0 andalso LastCC =/= 0 andalso LastCC > CC of
                true ->
                    NextI = case I > 1 of true -> I - 1; false -> I end,
                    sort_canonical(array:set(I - 1, CP,
                                             array:set(I, Last, Unicode)),
                                   NextI, Length);
                false ->
                    sort_canonical(Unicode, I + 1, Length)
            end
    end.


%% IDNA data functions
combining_class(C) ->
    case lookup(C) of
        false -> 0;
        Props ->
            erlang:list_to_integer(element(?COMBINING_CLASS, Props))
    end.

compat(C) ->
    lookup(C, fun(Props) ->
                      case element(?DECOMPOSITION, Props) of
                          [] -> undefined;
                          Val ->
                              Tokens = string:tokens(Val, " "),
                              CodePoints = dehex(case hd(Val) of
                                                     $< -> tl(Tokens);
                                                     _ -> Tokens
                                                 end),
                              CodePoints
                      end
              end).

composition(A, B) ->
    Key = lists:flatten([hex(A), " ", hex(B)]),
    case idna_unicode_data2:decomposition(Key) of
        false -> undefined;
        Val -> erlang:list_to_integer(Val, 16)
    end.


lowercase(C) ->
    lookup(C, fun(Props) ->
                case element(?LOWERCASE_MAPPING, Props) of
                    [] -> C;
                    Hex -> erlang:list_to_integer(Hex, 16)
                end
        end).

hex(Codepoint) ->
    string:right(erlang:integer_to_list(Codepoint, 16), 4, $0).

dehex(Strings) ->
    [erlang:list_to_integer(String, 16) || String <- Strings].

lookup(Codepoint) ->
    idna_unicode_data1:l(hex(Codepoint)).

lookup(Codepoint, Fun) ->
    case lookup(Codepoint) of
        false -> {error, bad_codepoint};
        Props -> Fun(Props)
    end.
