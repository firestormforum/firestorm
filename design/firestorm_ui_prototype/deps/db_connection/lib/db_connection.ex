defmodule DBConnection.Stream do
  defstruct [:conn, :query, :params, :opts]

  @type t :: %__MODULE__{conn: DBConnection.conn,
                         query: any,
                         params: any,
                         opts: Keyword.t}
end
defimpl Enumerable, for: DBConnection.Stream do
  def count(_), do: {:error, __MODULE__}

  def member?(_, _), do: {:error, __MODULE__}

  def reduce(stream, acc, fun), do: DBConnection.reduce(stream, acc, fun)
end

defmodule DBConnection.PrepareStream do
  defstruct [:conn, :query, :params, :opts]

  @type t :: %__MODULE__{conn: DBConnection.conn,
                         query: any,
                         params: any,
                         opts: Keyword.t}
end
defimpl Enumerable, for: DBConnection.PrepareStream do
  def count(_), do: {:error, __MODULE__}

  def member?(_, _), do: {:error, __MODULE__}

  def reduce(stream, acc, fun), do: DBConnection.reduce(stream, acc, fun)
end

defmodule DBConnection do
  @moduledoc """
  A behaviour module for implementing efficient database connection
  client processes, pools and transactions.

  `DBConnection` handles callbacks differently to most behaviours. Some
  callbacks will be called in the calling process, with the state
  copied to and from the calling process. This is useful when the data
  for a request is large and means that a calling process can interact
  with a socket directly.

  A side effect of this is that query handling can be written in a
  simple blocking fashion, while the connection process itself will
  remain responsive to OTP messages and can enqueue and cancel queued
  requests.

  If a request or series of requests takes too long to handle in the
  client process a timeout will trigger and the socket can be cleanly
  disconnected by the connection process.

  If a calling process waits too long to start its request it will
  timeout and its request will be cancelled. This prevents requests
  building up when the database can not keep up.

  If no requests are received for a period of time the connection will
  trigger an idle timeout and the database can be pinged to keep the
  connection alive.

  Should the connection be lost, attempts will be made to reconnect with
  (configurable) exponential random backoff to reconnect. All state is
  lost when a connection disconnects but the process is reused.

  The `DBConnection.Query` protocol provide utility functions so that
  queries can be prepared or encoded and results decoding without
  blocking the connection or pool.

  By default the `DBConnection` provides a single connection. However
  the `:pool` option can be set to use a pool of connections. If a
  pool is used the module must be passed as an option - unless inside a
  `run/3` or `transaction/3` fun and using the run/transaction
  connection reference (`t`).
  """
  defstruct [:pool_mod, :pool_ref, :conn_mod, :conn_ref]

  @typedoc """
  Run or transaction connection reference.
  """
  @type t :: %__MODULE__{pool_mod: module,
                         pool_ref: any,
                         conn_mod: any,
                         conn_ref: reference}
  @type conn :: GenServer.server | t
  @type query :: any
  @type params :: any
  @type result :: any
  @type cursor :: any

  @doc """
  Connect to the database. Return `{:ok, state}` on success or
  `{:error, exception}` on failure.

  If an error is returned it will be logged and another
  connection attempt will be made after a backoff interval.

  This callback is called in the connection process.
  """
  @callback connect(opts :: Keyword.t) ::
    {:ok, state :: any} | {:error, Exception.t}

  @doc """
  Checkouts the state from the connection process. Return `{:ok, state}`
  to allow the checkout or `{:disconnect, exception} to disconnect.

  This callback is called when the control of the state is passed to
  another process. `checkin/1` is called with the new state when control
  is returned to the connection process.

  Messages are discarded, instead of being passed to `handle_info/2`,
  when the state is checked out.

  This callback is called in the connection process.
  """
  @callback checkout(state :: any) ::
    {:ok, new_state :: any} | {:disconnect, Exception.t, new_state :: any}

  @doc """
  Checks in the state to the connection process. Return `{:ok, state}`
  to allow the checkin or `{:disconnect, exception}` to disconnect.

  This callback is called when the control of the state is passed back
  to the connection process. It should reverse any changes made in
  `checkout/2`.

  This callback is called in the connection process.
  """
  @callback checkin(state :: any) ::
    {:ok, new_state :: any} | {:disconnect, Exception.t, new_state :: any}

  @doc """
  Called when the connection has been idle for a period of time. Return
  `{:ok, state}` to continue or `{:disconnect, exception}` to
  disconnect.

  This callback is called if no callbacks have been called after the
  idle timeout and a client process is not using the state. The idle
  timeout can be configured by the `:idle_timeout` option. This function
  can be called whether the connection is checked in or checked out.

  This callback is called in the connection process.
  """
  @callback ping(state :: any) ::
    {:ok, new_state :: any} | {:disconnect, Exception.t, new_state :: any}

  @doc """
  Handle the beginning of a transaction. Return `{:ok, result, state}` to
  continue, `{:error, exception, state}` to abort the transaction and
  continue or `{:disconnect, exception, state}` to abort the transaction
  and disconnect.

  This callback is called in the client process.
  """
  @callback handle_begin(opts :: Keyword.t, state :: any) ::
    {:ok, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Handle commiting a transaction. Return `{:ok, result, state}` on success and
  to continue, `{:error, exception, state}` to abort the transaction and
  continue or `{:disconnect, exception, state}` to abort the transaction
  and disconnect.

  This callback is called in the client process.
  """
  @callback handle_commit(opts :: Keyword.t, state :: any) ::
    {:ok, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Handle rolling back a transaction. Return `{:ok, result, state}` on success
  and to continue, `{:error, exception, state}` to abort the transaction
  and continue or `{:disconnect, exception, state}` to abort the
  transaction and disconnect.

  A transaction will be rolled back if an exception occurs or
  `rollback/2` is called.

  This callback is called in the client process.
  """
  @callback handle_rollback(opts :: Keyword.t, state :: any) ::
    {:ok, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Prepare a query with the database. Return `{:ok, query, state}` where
  `query` is a query to pass to `execute/4` or `close/3`,
  `{:error, exception, state}` to return an error and continue or
  `{:disconnect, exception, state}` to return an error and disconnect.

  This callback is intended for cases where the state of a connection is
  needed to prepare a query and/or the query can be saved in the
  database to call later.

  This callback is called in the client process.
  """
  @callback handle_prepare(query, opts :: Keyword.t, state :: any) ::
    {:ok, query, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Execute a query prepared by `handle_prepare/3`. Return
  `{:ok, result, state}` to return the result `result` and continue,
  `{:error, exception, state}` to return an error and continue or
  `{:disconnect, exception, state}` to return an error and disconnect.

  This callback is called in the client process.
  """
  @callback handle_execute(query, params, opts :: Keyword.t, state :: any) ::
    {:ok, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Close a query prepared by `handle_prepare/3` with the database. Return
  `{:ok, result, state}` on success and to continue,
  `{:error, exception, state}` to return an error and continue, or
  `{:disconnect, exception, state}` to return an errior and disconnect.

  This callback is called in the client process.
  """
  @callback handle_close(query, opts :: Keyword.t, state :: any) ::
    {:ok, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Declare a cursor using a query prepared by `handle_prepare/3`. Return
  `{:ok, cursor, state}' to start a cursor for a stream and continue,
  `{:error, exception, state}` to return an error and continue or
  `{:disconnect, exception, state}` to return an error and disconnect.

  This callback is called in the client process.
  """
  @callback handle_declare(query, params, opts :: Keyword.t, state :: any) ::
    {:ok, cursor, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Fetch the first result from a cursor declared by `handle_declare/4`. Return
  `{:ok, result, state}` to return the result `result` and continue,
  `{:deallocate, result, state}` to return the result `result` and deallocate,
  `{:error, exception, state}` to return an error and close the cursor,
  `{:disconnect, exception, state}` to return an error and disconnect.

  This callback is called in the client process.
  """
  @callback handle_first(query, cursor, opts :: Keyword.t, state :: any) ::
    {:ok | :deallocate, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Fetch the next result from a cursor declared by `handle_declare/4`. Return
  `{:ok, result, state}` to return the result `result` and continue,
  `{:deallocate, result, state}` to return the result `result` and deallocate,
  `{:error, exception, state}` to return an error and close the cursor,
  `{:disconnect, exception, state}` to return an error and disconnect.

  This callback is called in the client process.
  """
  @callback handle_next(query, cursor, opts :: Keyword.t, state :: any) ::
    {:ok | :deallocate, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Deallocate a cursor declared by `handle_declare/4' with the database. Return
  `{:ok, result, state}` on success and to continue,
  `{:error, exception, state}` to return an error and continue, or
  `{:disconnect, exception, state}` to return an errior and disconnect.

  This callback is called in the client process.
  """
  @callback handle_deallocate(query, cursor, opts :: Keyword.t, state :: any) ::
    {:ok, result, new_state :: any} |
    {:error | :disconnect, Exception.t, new_state :: any}

  @doc """
  Handle a message received by the connection process when checked in.
  Return `{:ok, state}` to continue or `{:disconnect, exception,
  state}` to disconnect.

  Messages received by the connection process when checked out will be
  logged and discared.

  This callback is called in the connection process.
  """
  @callback handle_info(msg :: any, state :: any) ::
    {:ok, new_state :: any} |
    {:disconnect, Exception.t, new_state :: any}

  @doc """
  Disconnect from the database. Return `:ok`.

  The exception as first argument is the exception from a `:disconnect`
  3-tuple returned by a previous callback.

  If the state is controlled by a client and it exits or takes too long
  to process a request the state will be last known state. In these
  cases the exception will be a `DBConnection.ConnectionError`.

  This callback is called in the connection process.
  """
  @callback disconnect(err :: Exception.t, state :: any) :: :ok

  @doc """
  Use `DBConnection` to set the behaviour and include default
  no-op implementations for `ping/1` and `handle_info/2`.
  """
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour DBConnection

      def connect(_) do
        # We do this to trick dialyzer to not complain about non-local returns.
        message = "connect/1 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message)}
        end
      end

      def disconnect(_, _) do
        message = "disconnect/2 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> :ok
        end
      end

      def checkout(state) do
        message = "checkout/1 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:disconnect, RuntimeError.exception(message), state}
        end
      end

      def checkin(state) do
        message = "checkin/1 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:disconnect, RuntimeError.exception(message), state}
        end
      end

      def ping(state), do: {:ok, state}

      def handle_begin(_, state) do
        message = "handle_begin/2 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_commit(_, state) do
        message = "handle_commit/2 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_rollback(_, state) do
        message = "handle_rollback/2 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_prepare(_, _, state) do
       message = "handle_prepare/3 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_execute(_, _, _, state) do
        message = "handle_execute/4 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_close(_, _, state) do
        message = "handle_close/3 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_declare(_, _, _, state) do
       message = "handle_declare/4 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_first(_, _, _, state) do
       message = "handle_first/4 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_next(_, _, _, state) do
       message = "handle_next/4 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_deallocate(_, _, _, state) do
        message = "handle_deallocate/4 not implemented"
        case :erlang.phash2(1, 1) do
          0 -> raise message
          1 -> {:error, RuntimeError.exception(message), state}
        end
      end

      def handle_info(_, state), do: {:ok, state}

      defoverridable [connect: 1, disconnect: 2, checkout: 1, checkin: 1,
                      ping: 1, handle_begin: 2, handle_commit: 2,
                      handle_rollback: 2, handle_prepare: 3, handle_execute: 4,
                      handle_close: 3, handle_declare: 4, handle_first: 4,
                      handle_next: 4, handle_deallocate: 4, handle_info: 2]
    end
  end

  @doc """
  Ensures the given pool applications have been started.

  ### Options

    * `:pool` - The `DBConnection.Pool` module to use, (default:
    `DBConnection.Connection`)

  """
  @spec ensure_all_started(opts :: Keyword.t, type :: atom) ::
    {:ok, [atom]} | {:error, atom}
  def ensure_all_started(opts, type \\ :temporary) do
    Keyword.get(opts, :pool, DBConnection.Connection).ensure_all_started(opts, type)
  end

  @doc """
  Start and link to a database connection process.

  ### Options

    * `:pool` - The `DBConnection.Pool` module to use, (default:
    `DBConnection.Connection`)
    * `:idle` - The idle strategy, `:passive` to avoid checkin when idle and
    `:active` to checkin when idle (default: `:passive`)
    * `:idle_timeout` - The idle timeout to ping the database (default:
    `1_000`)
    * `:backoff_min` - The minimum backoff interval (default: `1_000`)
    * `:backoff_max` - The maximum backoff interval (default: `30_000`)
    * `:backoff_type` - The backoff strategy, `:stop` for no backoff and
    to stop, `:exp` for exponential, `:rand` for random and `:rand_exp` for
    random exponential (default: `:rand_exp`)
    * `:after_connect` - A function to run on connect using `run/3`, either
    a 1-arity fun, `{module, function, args}` with `DBConnection.t` prepended
    to `args` or `nil` (default: `nil`)
    * `:name` - A name to register the started process (see the `:name` option
    in `GenServer.start_link/3`).

  ### Example

      {:ok, conn} = DBConnection.start_link(mod, [idle_timeout: 5_000])
  """
  @spec start_link(module, opts :: Keyword.t) :: GenServer.on_start
  def start_link(conn_mod, opts) do
    pool_mod = Keyword.get(opts, :pool, DBConnection.Connection)
    apply(pool_mod, :start_link, [conn_mod, opts])
  end

  @doc """
  Create a supervisor child specification for a pool of connections.

  See `Supervisor.Spec` for child options (`child_opts`).
  """
  @spec child_spec(module, opts :: Keyword.t, child_opts :: Keyword.t) ::
    Supervisor.Spec.spec
  def child_spec(conn_mod, opts, child_opts \\ []) do
    pool_mod = Keyword.get(opts, :pool, DBConnection.Connection)
    apply(pool_mod, :child_spec, [conn_mod, opts, child_opts])
  end

  @doc """
  Prepare a query with a database connection for later execution and
  returns `{:ok, query}` on success or `{:error, exception}` if there was
  an error.

  The returned `query` can then be passed to `execute/3` and/or `close/3`

  ### Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (ignored when using a run/transaction
    connection, default: `15_000`)
    * `:log` - A function to log information about a call, either
    a 1-arity fun, `{module, function, args}` with `DBConnection.LogEntry.t`
    prepended to `args` or `nil`. See `DBConnection.LogEntry` (default: `nil`)

  The pool and connection module may support other options. All options
  are passed to `handle_prepare/3`.

  ### Example

      query         = %Query{statement: "SELECT id FROM table"}
      {:ok, query}  = DBConnection.prepare(conn, query)
      {:ok, result} = DBConnection.execute(conn, query, [])
      :ok           = DBConnection.close(conn, query)

  """
  @spec prepare(conn, query, opts :: Keyword.t) ::
    {:ok, query} | {:error, Exception.t}
  def prepare(conn, query, opts \\ []) do
    query = parse(:prepare, query, nil, opts)
    case run_prepare(conn, query, opts) do
      {{:ok, query} = ok, meter} ->
        log(:prepare, query, nil, meter, ok)
      {error, meter} ->
        log(:prepare, query, nil, meter, error)
    end
  end

  @doc """
  Prepare a query with a database connection and return the prepared
  query. An exception is raised on error.

  See `prepare/3`.
  """
  @spec prepare!(conn, query, opts :: Keyword.t) :: query
  def prepare!(conn, query, opts \\ []) do
    case prepare(conn, query, opts) do
      {:ok, result} -> result
      {:error, err} -> raise err
    end
  end

  @doc """
  Prepare a query and execute it with a database connection and return both the
  prepared query and the result, `{:ok, query, result}` on success or
  `{:error, exception}` if there was an error.

  The returned `query` can be passed to `execute/4` and `close/3`.

  ### Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (ignored when using a run/transaction
    connection, default: `15_000`)
    * `:log` - A function to log information about a call, either
    a 1-arity fun, `{module, function, args}` with `DBConnection.LogEntry.t`
    prepended to `args` or `nil`. See `DBConnection.LogEntry` (default: `nil`)

  ### Example

      query                = %Query{statement: "SELECT id FROM table WHERE id=$1"}
      {:ok, query, result} = DBConnection.prepare_execute(conn, query, [1])
      {:ok, result2}       = DBConnection.execute(conn, query, [2])
      :ok                  = DBConnection.close(conn, query)
   """
  @spec prepare_execute(conn, query, params, Keyword.t) ::
    {:ok, query, result} |
    {:error, Exception.t}
  def prepare_execute(conn, query, params, opts \\ []) do
    query = parse(:prepare_execute, query, params, opts)
    case run_prepare_execute(conn, query, params, opts) do
      {{:ok, query, result}, meter} ->
        decode(:prepare_execute, query, params, meter, result, opts)
      {error, meter} ->
        log(:prepare_execute, query, params, meter, error)
    end
  end

  @doc """
  Prepare a query and execute it with a database connection and return both the
  prepared query and result. An exception is raised on error.

  See `prepare_execute/4`.
  """
  @spec prepare_execute!(conn, query, Keyword.t) :: {query, result}
  def prepare_execute!(conn, query, params, opts \\ []) do
    case prepare_execute(conn, query, params, opts) do
      {:ok, query, result} -> {query, result}
      {:error, err}        -> raise err
    end
  end

  @doc """
  Execute a prepared query with a database connection and return
  `{:ok, result}` on success or `{:error, exception}` if there was an
  error.

  If the query is not prepared on the connection an attempt may be made to
  prepare it and then execute again.

  ### Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (ignored when using a run/transaction
    connection, default: `15_000`)
    * `:log` - A function to log information about a call, either
    a 1-arity fun, `{module, function, args}` with `DBConnection.LogEntry.t`
    prepended to `args` or `nil`. See `DBConnection.LogEntry` (default: `nil`)

  The pool and connection module may support other options. All options
  are passed to `handle_execute/4`.

  See `prepare/3`.
  """
  @spec execute(conn, query, params, opts :: Keyword.t) ::
    {:ok, result} | {:error, Exception.t}
  def execute(conn, query, params, opts \\ []) do
    encoded = encode(:execute, query, params, opts)
    case run_execute(conn, query, encoded, opts)  do
      {{:ok, query, result}, meter} ->
        decode(:execute, query, params, meter, result, opts)
      {error, meter} ->
        log(:execute, query, params, meter, error)
    end
  end

  @doc """
  Execute a prepared query with a database connection and return the
  result. Raises an exception on error.

  See `execute/4`
  """
  @spec execute!(conn, query, params, opts :: Keyword.t) :: result
  def execute!(conn, query, params, opts \\ []) do
    case execute(conn, query, params, opts) do
      {:ok, result} -> result
      {:error, err} -> raise err
    end
  end

  @doc """
  Close a prepared query on a database connection and return `{:ok, result}` on
  success or `{:error, exception}` on error.

  This function should be used to free resources held by the connection
  process and/or the database server.

  ## Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (ignored when using a run/transaction
    connection, default: `15_000`)
    * `:log` - A function to log information about a call, either
    a 1-arity fun, `{module, function, args}` with `DBConnection.LogEntry.t`
    prepended to `args` or `nil`. See `DBConnection.LogEntry` (default: `nil`)

  The pool and connection module may support other options. All options
  are passed to `handle_close/3`.

  See `prepare/3`.
  """
  @spec close(conn, query, opts :: Keyword.t) ::
    {:ok, result} | {:error, Exception.t}
  def close(conn, query, opts \\ []) do
    {result, meter} = run_close(conn, query, opts)
    log(:close, query, nil, meter, result)
  end

  @doc """
  Close a prepared query on a database connection and return the result. Raises
  an exception on error.

  See `close/3`.
  """
  @spec close!(conn, query, opts :: Keyword.t) :: result
  def close!(conn, query, opts \\ []) do
    case close(conn, query, opts) do
      {:ok, result} -> result
      {:error, err} -> raise err
    end
  end

  @doc """
  Acquire a lock on a connection and run a series of requests on it. The
  result of the fun is return inside an `:ok` tuple: `{:ok, result}`.

  To use the locked connection call the request with the connection
  reference passed as the single argument to the `fun`. If the
  connection disconnects all future calls using that connection
  reference will fail.

  `run/3` and `transaction/3` can be nested multiple times but a
  `transaction/3` call inside another `transaction/3` will be treated
  the same as `run/3`.

  ### Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (default: `15_000`)

  The pool may support other options.

  ### Example

      {:ok, res} = DBConnection.run(conn, fn(conn) ->
        DBConnection.execute!(conn, "SELECT id FROM table", [])
      end)
  """
  @spec run(conn, (t -> result), opts :: Keyword.t) :: result when result: var
  def run(conn, fun, opts \\ [])
  def run(%DBConnection{} = conn, fun, _) do
    _ = fetch_info(conn)
    fun.(conn)
  end
  def run(pool, fun, opts) do
    {conn, conn_state} = checkout(pool, opts)
    put_info(conn, :idle, conn_state)
    run_begin(conn, fun, opts)
  end

  @doc """
  Acquire a lock on a connection and run a series of requests inside a
  transaction. The result of the transaction fun is return inside an `:ok`
  tuple: `{:ok result}`.

  To use the locked connection call the request with the connection
  reference passed as the single argument to the `fun`. If the
  connection disconnects all future calls using that connection
  reference will fail.

  `run/3` and `transaction/3` can be nested multiple times. If a transaction is
  rolled back or a nested transaction `fun` raises the transaction is marked as
  failed. Any calls inside a failed transaction (except `rollback/2`) will raise
  until the outer transaction call returns. All running `transaction/3` calls
  will return `{:error, :rollback}` if the transaction failed or connection
  closed and `rollback/2` is not called for that `transaction/3`.

  ### Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (default: `15_000`)
    * `:log` - A function to log information about begin, commit and rollback
    calls made as part of the transaction, either a 1-arity fun,
    `{module, function, args}` with `DBConnection.LogEntry.t` prepended to
    `args` or `nil`. See `DBConnection.LogEntry` (default: `nil`)

  The pool and connection module may support other options. All options
  are passed to `handle_begin/2`, `handle_commit/2` and
  `handle_rollback/2`.

  ### Example

      {:ok, res} = DBConnection.transaction(conn, fn(conn) ->
        DBConnection.execute!(conn, "SELECT id FROM table", [])
      end)
  """
  @spec transaction(conn, (conn -> result), opts :: Keyword.t) ::
    {:ok, result} | {:error, reason :: any} when result: var
  def transaction(conn, fun, opts \\ []) do
    {result, log_info} = transaction_meter(conn, fun, opts)
    transaction_log(log_info)
    case result do
      {:raise, err} ->
        raise err
      {kind, reason, stack} ->
        :erlang.raise(kind, reason, stack)
      other ->
        other
    end
  end

  @doc """
  Rollback a transaction, does not return.

  Aborts the current transaction fun. If inside `transaction/3` bubbles
  up to the top level.

  ### Example

      {:error, :bar} = DBConnection.transaction(conn, fn(conn) ->
        DBConnection.rollback(conn, :bar)
        IO.puts "never reaches here!"
      end)
  """
  @spec rollback(t, reason :: any) :: no_return
  def rollback(%DBConnection{conn_ref: conn_ref} = conn, err) do
    case get_info(conn) do
      {transaction, _} when transaction in [:transaction, :failed] ->
        throw({:rollback, conn_ref, err})
      {transaction, _, _} when transaction in [:transaction, :failed] ->
        throw({:rollback, conn_ref, err})
      {:idle, _} ->
        raise "not inside transaction"
      {:idle, _, _} ->
        raise "not inside transaction"
      :closed ->
        raise DBConnection.ConnectionError, "connection is closed"
    end
  end

  @doc """
  Create a stream that will prepare a query, execute it and stream results
  using a cursor.

  ### Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (ignored when using a run/transaction
    connection, default: `15_000`)
    * `:log` - A function to log information about a call, either
    a 1-arity fun, `{module, function, args}` with `DBConnection.LogEntry.t`
    prepended to `args` or `nil`. See `DBConnection.LogEntry` (default: `nil`)

  The pool and connection module may support other options. All options
  are passed to `handle_prepare/3, `handle_close/3, `handle_declare/4`,
  `handle_first/4`, `handle_next/4' and `handle_deallocate/4`.

  ### Example

      {:ok, results} = DBConnection.transaction(conn, fn(conn) ->
        query  = %Query{statement: "SELECT id FROM table"}
        stream = DBConnection.prepare_stream(conn, query, [])
        Enum.to_list(stream)
      end)
  """
  @spec prepare_stream(t, query, params, opts :: Keyword.t) ::
    DBConnection.PrepareStream.t
  def prepare_stream(%DBConnection{} = conn, query, params, opts) do
    %DBConnection.PrepareStream{conn: conn, query: query, params: params,
                                opts: opts}
  end

  @doc """
  Create a stream that will execute a prepared query and stream results using a
  cursor.

  ### Options

    * `:pool_timeout` - The maximum time to wait for a reply when making a
    synchronous call to the pool (default: `5_000`)
    * `:queue` - Whether to block waiting in an internal queue for the
    connection's state (boolean, default: `true`)
    * `:timeout` - The maximum time that the caller is allowed the
    to hold the connection's state (ignored when using a run/transaction
    connection, default: `15_000`)
    * `:log` - A function to log information about a call, either
    a 1-arity fun, `{module, function, args}` with `DBConnection.LogEntry.t`
    prepended to `args` or `nil`. See `DBConnection.LogEntry` (default: `nil`)

  The pool and connection module may support other options. All options
  are passed to `handle_declare/4`, `handle_first/4` , `handle_next/4 and
  `handle_deallocate/4`.

  ### Example

      {:ok, results} = DBConnection.transaction(conn, fn(conn) ->
        query  = %Query{statement: "SELECT id FROM table"}
        query  = DBConnection.prepare!(conn, query)
        stream = DBConnection.stream(conn, query, [])
        Enum.to_list(stream)
      end)
  """
  @spec stream(t, query, params, opts :: Keyword.t) :: DBConnection.Stream.t
  def stream(%DBConnection{} = conn, query, params, opts \\ []) do
    %DBConnection.Stream{conn: conn, query: query, params: params, opts: opts}
  end

  @doc false
  def reduce(%DBConnection.PrepareStream{} = stream, acc, fun) do
    %DBConnection.PrepareStream{conn: conn, query: query, params: params,
                                opts: opts} = stream
    start = &prepare_declare(&1, query, params, &2)
    resource(conn, start, &fetch/3, &deallocate/3, opts).(acc, fun)
  end
  def reduce(%DBConnection.Stream{} = stream, acc, fun) do
    %DBConnection.Stream{conn: conn, query: query, params: params,
                         opts: opts} = stream
    start = &declare(&1, query, params, &2)
    resource(conn, start, &fetch/3, &deallocate/3, opts).(acc, fun)
  end

  ## Helpers

  defp checkout(pool, opts) do
    pool_mod = Keyword.get(opts, :pool, DBConnection.Connection)
    case apply(pool_mod, :checkout, [pool, opts]) do
      {:ok, pool_ref, conn_mod, conn_state} ->
        conn = %DBConnection{pool_mod: pool_mod, pool_ref: pool_ref,
          conn_mod: conn_mod, conn_ref: make_ref()}
        {conn, conn_state}
      {:error, err} ->
        raise err
    end
  end

  defp checkin(conn, conn_state, opts) do
    %DBConnection{pool_mod: pool_mod, pool_ref: pool_ref} = conn
    _ = apply(pool_mod, :checkin, [pool_ref, conn_state, opts])
    :ok
  end

  defp delete_disconnect(conn, conn_state, err, opts) do
    _ = delete_info(conn)
    %DBConnection{pool_mod: pool_mod, pool_ref: pool_ref} = conn
    args = [pool_ref, err, conn_state, opts]
    _ = apply(pool_mod, :disconnect, args)
    :ok
  end

  defp delete_stop(conn, conn_state, kind, reason, stack, opts) do
    _ = delete_info(conn)
    msg = "client #{inspect self()} stopped: " <>
      Exception.format(kind, reason, stack)
    exception = DBConnection.ConnectionError.exception(msg)
    %DBConnection{pool_mod: pool_mod, pool_ref: pool_ref} = conn
    args = [pool_ref, exception, conn_state, opts]
    _ = apply(pool_mod, :stop, args)
    :ok
  end

  defp handle(%DBConnection{conn_mod: conn_mod} = conn, fun, args, opts) do
    {status, conn_state} = fetch_info(conn)
    try do
      apply(conn_mod, fun, args ++ [opts, conn_state])
    else
      {:ok, result, conn_state} ->
        put_info(conn, status, conn_state)
        {:ok, result}
      {:deallocate, _, conn_state} = deallocate
          when fun in [:handle_first, :handle_next] ->
        put_info(conn, status, conn_state)
        Tuple.delete_at(deallocate, 2)
      {:error, _, conn_state} = error ->
        put_info(conn, status, conn_state)
        Tuple.delete_at(error, 2)
      {:disconnect, err, conn_state} ->
        delete_disconnect(conn, conn_state, err, opts)
        {:error, err}
      other ->
        try do
          raise DBConnection.ConnectionError, "bad return value: #{inspect other}"
        catch
          :error, reason ->
            stack = System.stacktrace()
            delete_stop(conn, conn_state, :error, reason, stack, opts)
            {:error, reason, stack}
        end
    catch
      kind, reason ->
        stack = System.stacktrace()
        delete_stop(conn, conn_state, kind, reason, stack, opts)
        {kind, reason, stack}
    end
  end

  defp parse(call, query, params, opts) do
    try do
      DBConnection.Query.parse(query, opts)
    catch
      kind, reason ->
        pre_log(call, query, params, opts, kind, reason, System.stacktrace())
    end
  end

  defp encode(call, query, params, opts) do
    try do
      DBConnection.Query.encode(query, params, opts)
    catch
      kind, reason ->
        stack = System.stacktrace()
        pre_log(call, query, params, opts, kind, reason, stack)
    end
  end

  defp decode(call, query, params, meter, result, opts) do
    try do
      DBConnection.Query.decode(query, result, opts)
    catch
      kind, reason ->
        raised = {kind, reason, System.stacktrace()}
        decode_log(call, query, params, meter, raised)
    else
      result when call == :prepare_execute ->
        ok = {:ok, query, result}
        decode_log(call, query, params, meter, ok)
      result when call in [:execute, :first, :next] ->
        ok = {:ok, result}
        decode_log(call, query, params, meter, ok)
    end
  end

  defp pre_log(call, query, params, opts, kind, reason, stack) do
    case Keyword.get(opts, :log) do
      nil -> :erlang.raise(kind, reason, stack)
      log -> log(call, query, params, {log, []}, {kind, reason, stack})
    end
  end

  defp run_prepare(conn, query, opts) do
    run_meter(conn, fn(conn2) ->
      case handle(conn2, :handle_prepare, [query], opts) do
        {:ok, query} ->
          describe(conn2, query, opts)
        other ->
          other
      end
    end, opts)
  end

  defp describe(conn, query, opts) do
    try do
      DBConnection.Query.describe(query, opts)
    catch
      kind, reason ->
        raised = {kind, reason, System.stacktrace()}
        raised_close(conn, query, opts, raised)
    else
      query ->
        {:ok, query}
    end
  end

  defp run_prepare_execute(conn, query, params, opts) do
    run_meter(conn, fn(conn2) ->
      case handle(conn2, :handle_prepare, [query], opts) do
        {:ok, query} ->
          describe_run(conn2, :handle_execute, query, params, opts)
        other ->
          other
      end
    end, opts)
  end

  defp describe_run(conn, fun, query, params, opts) do
    try do
      query = DBConnection.Query.describe(query, opts)
      [query, DBConnection.Query.encode(query, params, opts)]
    catch
      kind, reason ->
        raised = {kind, reason, System.stacktrace()}
        raised_close(conn, query, opts, raised)
    else
      [query, _params] = args ->
        case handle(conn, fun, args, opts) do
          {:ok, result} ->
            {:ok, query, result}
          other ->
            other
        end
    end
  end

  defp raised_close(conn, query, opts, raised) do
    case handle(conn, :handle_close, [query], opts) do
      {:ok, _} ->
        raised
      {:error, _} ->
        raised
      {_kind, _reason, _stack} = raised ->
        raised
    end
  end

  defp run_execute(conn, query, params, opts) do
    run_meter(conn, fn(conn2) ->
      case handle(conn2, :handle_execute, [query, params], opts) do
        {:ok, result} ->
          {:ok, query, result}
        other ->
          other
      end
    end, opts)
  end

  defp run_close(conn, query, opts) do
    fun = &handle(&1, :handle_close, [query], opts)
    run_meter(conn, fun, opts)
  end

  defmacrop time() do
    if function_exported?(:erlang, :monotonic_time, 0) do
      quote do: :erlang.monotonic_time()
    else
      quote do: :os.timestamp()
    end
  end

  defp run_meter(%DBConnection{} = conn, fun, opts) do
    case Keyword.get(opts, :log) do
      nil ->
        {run(conn, fun, opts), nil}
      log ->
        run_meter(conn, log, [], fun, opts)
      end
  end
  defp run_meter(pool, fun, opts) do
    case Keyword.get(opts, :log) do
      nil ->
        {run(pool, fun, opts), nil}
      log ->
        run_meter(pool, log, [checkout: time()], fun, opts)
    end
  end

  defp run_meter(conn, log, times, fun, opts) do
    fun = fn(conn2) ->
      start = time()
      result = fun.(conn2)
      stop = time()
      meter = {log, [stop: stop, start: start] ++ times}
      {result, meter}
    end
    run(conn, fun, opts)
  end

  defp decode_log(_, _, _, nil, result), do: log_result(result)
  defp decode_log(call, query, params, {log, times}, result) do
   log(call, query, params, log, [decode: time()] ++ times, result)
  end

  defp transaction_log(nil), do: :ok
  defp transaction_log({log, times, callback, result}) do
    call = transaction_call(callback)
    result = transaction_result(result)
    _ = log(:transaction, call, nil, log, times, result)
    :ok
  end

  defp transaction_call(:handle_begin), do: :begin
  defp transaction_call(:handle_commit), do: :commit
  defp transaction_call(:handle_rollback), do: :rollback

  defp transaction_result({:ok, _} = ok), do: ok
  defp transaction_result({:raise, err}), do: {:error, err}
  defp transaction_result({_kind, _reason, _stack} = raised), do: raised

  defp log(_, _, _, nil, result), do: log_result(result)
  defp log(call, query, params, {log, times}, result) do
    log(call, query, params, log, times, result)
  end

  defp log(call, query, params, log, times, result) do
    entry = DBConnection.LogEntry.new(call, query, params, times, entry_result(result))
    log(log, entry)
    log_result(result)
  end

  defp entry_result({kind, reason, stack})
  when kind in [:error, :exit, :throw] do
    msg = "an exception was raised: " <> Exception.format(kind, reason, stack)
    {:error, %DBConnection.ConnectionError{message: msg}}
  end
  defp entry_result(other), do: other

  defp log({mod, fun, args}, entry), do: apply(mod, fun, [entry | args])
  defp log(fun, entry), do: fun.(entry)

  defp log_result({kind, reason, stack}) when kind in [:error, :exit, :throw] do
    :erlang.raise(kind, reason, stack)
  end
  defp log_result(other), do: other

  defp run_begin(conn, fun, opts) do
    try do
      fun.(conn)
    after
      run_end(conn, opts)
    end
  end

  defp run_end(conn, opts) do
    case delete_info(conn) do
      {:idle, conn_state} ->
        checkin(conn, conn_state, opts)
      {status, conn_state} when status in [:transaction, :failed] ->
        try do
          raise "connection run ended in transaction"
        catch
          :error, reason ->
            stack = System.stacktrace()
            delete_stop(conn, conn_state, :error, reason, stack, opts)
            :erlang.raise(:error, reason, stack)
        end
      :closed ->
        :ok
    end
  end

  defp transaction_meter(%DBConnection{} = conn, fun, opts) do
    case fetch_info(conn) do
      {:transaction, _} ->
        {transaction_nested(conn, fun), nil}
      {:idle, conn_state} ->
        log = Keyword.get(opts, :log)
        begin_meter(conn, conn_state, log, [], fun, opts)
    end
  end
  defp transaction_meter(pool, fun, opts) do
    case Keyword.get(opts, :log) do
      nil ->
        run(pool, &begin(&1, nil, [], fun, opts), opts)
      log ->
        times = [checkout: time()]
        run(pool, &begin(&1, log, times, fun, opts), opts)
    end
  end

  defp begin(conn, log, times, fun, opts) do
    {:idle, conn_state} = get_info(conn)
    begin_meter(conn, conn_state, log, times, fun, opts)
  end

  defp begin_meter(conn, conn_state, nil, [], fun, opts) do
    case handle(conn, conn_state, :handle_begin, opts, :transaction) do
      {:ok, _} ->
        transaction_run(conn, nil, fun, opts)
      error ->
        {error, nil}
    end
  end
  defp begin_meter(conn, conn_state, log, times, fun, opts) do
    start = time()
    result = handle(conn, conn_state, :handle_begin, opts, :transaction)
    stop = time()
    log_info = {log, [stop: stop, start: start] ++ times, :handle_begin, result}
    case result do
      {:ok, _} ->
        fun = fn(conn2) ->
          transaction_log(log_info)
          fun.(conn2)
        end
        transaction_run(conn, log, fun, opts)
      error ->
        {error, log_info}
    end
  end

  defp transaction_run(conn, log, fun, opts) do
    %DBConnection{conn_ref: conn_ref} = conn
    try do
      fun.(conn)
    else
      result ->
        result = {:ok, result}
        commit(conn, log, opts, result)
    catch
      :throw, {:rollback, ^conn_ref, reason} ->
        result = {:error, reason}
        rollback(conn, log, opts, result)
      kind, reason ->
        result = {kind, reason, System.stacktrace()}
        rollback(conn, log, opts, result)
    end
  end

  defp commit(conn, log, opts, result) do
    case get_info(conn) do
      {:transaction, conn_state} ->
        conclude_meter(conn, conn_state, log, :handle_commit, opts, result)
      {:failed, conn_state} ->
        result = {:error, :rollback}
        conclude_meter(conn, conn_state, log, :handle_rollback, opts, result)
      :closed ->
        {{:error, :rollback}, nil}
    end
  end

  defp rollback(conn, log, opts, result) do
    case get_info(conn) do
      {trans, conn_state} when trans in [:transaction, :failed] ->
        conclude_meter(conn, conn_state, log, :handle_rollback, opts, result)
      :closed ->
        {result, nil}
    end
  end

  defp conclude_meter(conn, conn_state, nil, callback, opts, result) do
    case handle(conn, conn_state, callback, opts, :idle) do
      {:ok, _} ->
        {result, nil}
      error ->
        {error, nil}
    end
  end
  defp conclude_meter(conn, conn_state, log, callback, opts, result) do
    start = time()
    cb_result = handle(conn, conn_state, callback, opts, :idle)
    stop = time()
    times = [stop: stop, start: start]
    case cb_result do
      {:ok, _} ->
        {result, {log, times, callback, cb_result}}
      _error ->
        {cb_result, {log, times, callback, cb_result}}
    end
  end

  defp handle(conn, conn_state, callback, opts, status) do
    %DBConnection{conn_mod: conn_mod} = conn
    try do
      apply(conn_mod, callback, [opts, conn_state])
    else
      {:ok, result, conn_state} ->
        put_info(conn, status, conn_state)
        {:ok, result}
      {:error, err, conn_state} ->
        put_info(conn, :idle, conn_state)
        {:raise, err}
      {:disconnect, err, conn_state} ->
        delete_disconnect(conn, conn_state, err, opts)
        {:raise, err}
      other ->
        try do
          raise DBConnection.ConnectionError, "bad return value: #{inspect other}"
        catch
          :error, reason ->
            stack = System.stacktrace()
            delete_stop(conn, conn_state, :error, reason, stack, opts)
            {:error, reason, stack}
        end
    catch
      kind, reason ->
        stack = System.stacktrace()
        delete_stop(conn, conn_state, kind, reason, stack, opts)
        {kind, reason, stack}
    end
  end

  defp transaction_nested(conn, fun) do
    %DBConnection{conn_ref: conn_ref} = conn
    try do
      fun.(conn)
    else
      result ->
        transaction_ok(conn, {:ok, result})
    catch
      :throw, {:rollback, ^conn_ref, reason} ->
        transaction_failed(conn)
        {:error, reason}
      kind, reason ->
        stack = System.stacktrace()
        transaction_failed(conn)
        :erlang.raise(kind, reason, stack)
    end
  end

  defp transaction_ok(conn, result) do
    case get_info(conn) do
      {:failed, _} ->
        {:error, :rollback}
      _ ->
        result
    end
  end

  defp transaction_failed(conn) do
    case get_info(conn) do
      {:transaction, conn_state} ->
        put_info(conn, :failed, conn_state)
      _ ->
        :ok
    end
  end

  defp prepare_declare(conn, query, params, opts) do
    query = parse(:prepare_declare, query, params, opts)
    case run_prepare_declare(conn, query, params, opts) do
      {{:ok, query, cursor}, meter} ->
        prepare_declare_log(conn, query, params, meter, cursor, opts)
      {error, meter} ->
        {:error, err} = log(:prepare_declare, query, params, meter, error)
        raise err
    end
  end

  defp run_prepare_declare(conn, query, params, opts) do
    run_meter(conn, fn(conn2) ->
      case handle(conn2, :handle_prepare, [query], opts) do
        {:ok, query} ->
          describe_run(conn2, :handle_declare, query, params, opts)
        other ->
          other
      end
    end, opts)
  end

  defp prepare_declare_log(conn, query, params, meter, cursor, opts) do
    try do
      log(:prepare_declare, query, params, meter, {:ok, query, cursor})
    catch
      kind, reason ->
        stack = System.stacktrace()
        deallocate(conn, query, cursor, opts)
        :erlang.raise(kind, reason, stack)
    else
      {:ok, query, cursor} ->
        {:first, query, cursor}
    end
  end

  defp declare(conn, query, params, opts) do
    encoded = encode(:declare, query, params, opts)
    case run_declare(conn, query, encoded, opts) do
      {{:ok, cursor}, meter} ->
        declare_log(conn, query, params, meter, cursor, opts)
      {error, meter} ->
        {:error, err} = log(:declare, query, params, meter, error)
        raise err
    end
  end

  defp run_declare(conn, query, params, opts) do
    run_meter(conn, &handle(&1, :handle_declare, [query, params], opts), opts)
  end

  defp declare_log(conn, query, params, meter, cursor, opts) do
    try do
      log(:declare, query, params, meter, {:ok, cursor})
    catch
      kind, reason ->
        stack = System.stacktrace()
        deallocate(conn, query, cursor, opts)
        :erlang.raise(kind, reason, stack)
    else
      {:ok, cursor} ->
        {:first, query, cursor}
    end
  end

  defp fetch(conn, {:first, query, cursor}, opts) do
    fetch(conn, :handle_first, :first, query, cursor, opts)
  end
  defp fetch(conn, {:next, query, cursor}, opts) do
    fetch(conn, :handle_next, :next, query, cursor, opts)
  end
  defp fetch(_, {:deallocate, _,  _} = state, _) do
    {:halt, state}
  end

  def fetch(conn, fun, call, query, cursor, opts) do
    fetch = &handle(&1, fun, [query, cursor], opts)
    case run_meter(conn, fetch, opts) do
      {{:ok, result}, meter} ->
        fetch_decode(:next, call, query, cursor, meter, result, opts)
      {{:deallocate, result}, meter} ->
        fetch_decode(:deallocate, call, query, cursor, meter, result, opts)
      {error, meter} ->
        {:error, err} = log(call, query, cursor, meter, error)
        raise err
    end
  end

  defp fetch_decode(status, call, query, cursor, meter, result, opts) do
    {:ok, decoded} = decode(call, query, cursor, meter, result, opts)
    {[decoded], {status, query, cursor}}
  end

  defp deallocate(conn, {_, query, cursor}, opts) do
    case get_info(conn) do
      :closed -> :ok
      _       -> deallocate(conn, query, cursor, opts)
    end
  end

  defp deallocate(conn, query, cursor, opts) do
    close = &handle(&1, :handle_deallocate, [query, cursor], opts)
    {result, meter} = run_meter(conn, close, opts)
    case log(:deallocate, query, cursor, meter, result) do
      {:ok, _}      -> :ok
      {:error, err} -> raise err
    end
  end

  defp resource(%DBConnection{} = conn, start, next, stop, opts) do
    start = fn() -> start.(conn, opts) end
    next = fn(state) -> next.(conn, state, opts) end
    stop = fn(state) -> stop.(conn, state, opts) end
    Stream.resource(start, next, stop)
  end

  defp put_info(conn, status, conn_state) do
    _ = Process.put(key(conn), {status, conn_state})
    :ok
  end

  defp fetch_info(conn) do
    case get_info(conn) do
      {:failed, _} ->
        raise DBConnection.ConnectionError, "transaction rolling back"
      {_, _} = info ->
        info
      :closed ->
        raise DBConnection.ConnectionError, "connection is closed"
    end
  end

  defp get_info(conn), do: Process.get(key(conn), :closed)

  defp delete_info(conn) do
    Process.delete(key(conn)) || :closed
  end

  defp key(%DBConnection{conn_ref: conn_ref}), do: {__MODULE__, conn_ref}
end
