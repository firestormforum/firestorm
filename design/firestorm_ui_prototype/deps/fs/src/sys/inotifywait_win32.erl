-module(inotifywait_win32).
-include("api.hrl").
-export(?API).

find_executable() ->
    fs:find_executable("inotifywait.exe", "deps/fs/priv/inotifywait.exe").

known_events() -> [created, modified, removed, renamed, undefined].

start_port(Path, Cwd) ->
    Path1 = filename:absname(Path),
    Args = ["-m", "-r", Path1],
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, {line, 16384}, {args, Args}, {cd, Cwd}]).

line_to_event(Line) ->
    {match, [Dir, Flags1, DirEntry]} = re:run(Line, re(), [{capture, all_but_first, list}]),
    Flags = [convert_flag(F) || F <- string:tokens(Flags1, ",")],
    Path = filename:join(Dir,DirEntry),
    {Path, Flags}.

    
convert_flag("CREATE") -> created;
convert_flag("MODIFY") -> modified;
convert_flag("DELETE") -> removed;
convert_flag("MOVE") -> renamed;
convert_flag(_) -> undefined.

re() ->
    case get(inotifywait_re) of
        undefined ->
            {ok, R} = re:compile("^(.*\\\\.*) ([A-Z_,]+) (.*)$", [unicode]),
            put(inotifywait_re, R),
            R;
        V -> V
    end.

