-module(fs_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

-record(state, {port, path, backend}).

notify(file_event = A, Msg) -> Key = {fs, A}, gen_event:notify(fs_events, {self(), Key, Msg}).
start_link(Backend, Path, Cwd) -> gen_server:start_link({local, ?SERVER}, ?MODULE, [Backend, Path, Cwd], []).
init([Backend, Path, Cwd]) -> {ok, #state{port=Backend:start_port(Path, Cwd),path=Path,backend=Backend}}.

handle_call(known_events, _From, #state{backend=Backend} = State) -> {reply, Backend:known_events(), State};
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({_Port, {data, {eol, Line}}}, #state{backend=Backend} = State) ->
    Event = Backend:line_to_event(Line),
    notify(file_event, Event),
    {noreply, State};
handle_info({_Port, {data, {noeol, Line}}}, State) ->
    error_logger:error_msg("~p line too long: ~p, ignoring~n", [?SERVER, Line]),
    {noreply, State};
handle_info({_Port, {exit_status, Status}}, State) -> {stop, {port_exit, Status}, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, #state{port=Port}) -> (catch port_close(Port)), ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
