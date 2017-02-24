-module('certifi').

%% API exports
-export([cacerts/0]).
-export([weak_cacerts/0]).

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate that can then be used via the cacerts setting in ssl options
%% passed to the connect function.
cacerts() ->
    certifi_cacerts:ders().


%% @doc deprecated bundle. fix 1024-bit root issues
weak_cacerts() ->
    certifi_weakcerts:ders().