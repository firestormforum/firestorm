defmodule DBConnection.Pool do
  @moduledoc """
  A behaviour module for implementing a pool of database connections
  using `DBConnection`.
  """

  @doc """
  Ensure all applications necessary to run the pool are started.
  """
  @callback ensure_all_started(opts :: Keyword.t, type :: :application.restart_type) ::
    {:ok, [atom]} | {:error, atom}

  @doc """
  Start and link to a pool of `module` connections with options `opts`.
  """
  @callback start_link(module, opts :: Keyword.t) ::
    GenServer.on_start

  @doc """
  Create a supervisor child specification for the pool with module
  `module`, options `opts` and child specification options `child_opts`.
  """
  @callback child_spec(module, opts :: Keyword.t, child_opts :: Keyword.t) ::
    Supervisor.Spec.spec

  @doc """
  Checkout a connection's state from a pool.

  The returned `pool_ref` will be passed to `checkin/3`, `disconnect/4`
  and `stop/4`.

  `module` and `state` are the module and state of the connection.
  """
  @callback checkout(pool :: GenServer.server, opts :: Keyword.t) ::
    {:ok, pool_ref :: any, module, state :: any} | {:error, Exception.t}

  @doc """
  Checkin a connection's state to the pool.

  The `pool_ref` is from the return of `checkout/2`.

  `state` is the lastest state of the connection.
  """
  @callback checkin(pool_ref :: any, state :: any, opts :: Keyword.t) :: :ok

  @doc """
  Checkin a connection's state to the pool and disconnect it with an
  exception.

  The `pool_ref` is from the return of `checkout/2`.

  `state` is the lastest state of the connection.
  """
  @callback disconnect(pool_ref :: any, err :: Exception.t, state :: any, opts :: Keyword.t) ::
    :ok

  @doc """
  Stop a connection.

  The `pool_ref` is from the return of `checkout/2`.

  `state` is the lastest state of the connection.
  """
  @callback stop(pool_ref :: any, err :: Exception.t, state :: any, opts :: Keyword.t) ::
    :ok
end
