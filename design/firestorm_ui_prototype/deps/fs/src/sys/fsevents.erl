-module(fsevents).
-include("api.hrl").
-export(?API).

find_executable() ->
    fs:find_executable("mac_listener", "deps/fs/priv/mac_listener").

known_events() ->
    [mustscansubdirs,userdropped,kerneldropped,eventidswrapped,historydone,rootchanged,
        mount,unmount,created,removed,inodemetamod,renamed,modified,finderinfomod,changeowner,
        xattrmod,isfile,isdir,issymlink,ownevent].

start_port(Path, Cwd) ->
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, {line, 16384}, {args, ["-F", Path]}, {cd, Cwd}]).

line_to_event(Line) ->
    [_EventId, Flags1, Path] = string:tokens(Line, [$\t]),
    [_, Flags2] = string:tokens(Flags1, [$=]),
    {ok, T, _} = erl_scan:string(Flags2 ++ "."),
    {ok, Flags} = erl_parse:parse_term(T),
    {Path, Flags}.
