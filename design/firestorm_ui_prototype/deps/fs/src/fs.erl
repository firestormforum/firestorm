-module(fs).
-include_lib("kernel/include/file.hrl").
-export([subscribe/0, known_events/0, start_looper/0, path/0, find_executable/2]).

% sample subscriber

subscribe() -> gen_event:add_sup_handler(fs_events, {fs_event_bridge, self()}, [self()]).
known_events() -> gen_server:call(fs_server, known_events).
start_looper() -> spawn(fun() -> subscribe(), loop() end).

path() ->
    case application:get_env(fs, path) of
        {ok, P} -> filename:absname(P);
        undefined -> filename:absname("") end.

loop() ->
    receive
        {_Pid, {fs, file_event}, {Path, Flags}} -> error_logger:info_msg("file_event: ~p ~p", [Path, Flags]);
        _ -> ignore end,
    loop().

find_executable(Cmd, DepsPath) ->
    case priv_file(Cmd) of
    false -> mad_file(DepsPath);
    Priv  -> Priv end.

mad_file(DepsPath) ->
    case filelib:is_regular(DepsPath) of
    true  -> DepsPath;
    false ->
        case mad_repl:load_file(DepsPath) of
        {error,_} ->
            %% This path has been already checked in find_executable/2
            false;
        {ok,ETSFile} ->
            filelib:ensure_dir(DepsPath),
            file:write_file(DepsPath, ETSFile),
            file:write_file_info(DepsPath, #file_info{mode=8#00555}) end end.

priv_file(Cmd) ->
    case code:priv_dir(fs) of
    Priv when is_list(Priv) ->
        Path = filename:join(Priv, Cmd),
        case filelib:is_regular(Path) of
        true  -> Path;
        false -> false end;
    _ ->
        false end.
