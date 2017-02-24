-module(idna).

-export([to_ascii/1,
         utf8_to_ascii/1,
         from_ascii/1]).

-define(ACE_PREFIX, "xn--").

to_ascii(Domain) ->
    to_ascii(string:tokens(idna_unicode:downcase(Domain), "."), []).

utf8_to_ascii(Domain) ->
    to_ascii(idna_ucs:from_utf8(Domain)).

-spec from_ascii(nonempty_string()) -> nonempty_string().
from_ascii(Domain) ->
    from_ascii(string:tokens(Domain, "."), []).

%% Helper functions
%%
to_ascii([], Acc) ->
    lists:reverse(Acc);
to_ascii([Label|Labels], []) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label)));
to_ascii([Label|Labels], Acc) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label), [$.|Acc])).

label_to_ascii(Label) ->
    case lists:all(fun(C) -> idna_ucs:is_ascii(C) end, Label) of
        true ->
            Label;
        false ->
            ?ACE_PREFIX ++ punycode:encode(idna_unicode:normalize_kc(Label))
    end.

from_ascii([], Acc) ->
    lists:reverse(Acc);
from_ascii([Label|Labels], []) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label)));
from_ascii([Label|Labels], Acc) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label), [$.|Acc])).

label_from_ascii(?ACE_PREFIX ++ Label) ->
    punycode:decode(Label);
label_from_ascii(Label) ->
    Label.
