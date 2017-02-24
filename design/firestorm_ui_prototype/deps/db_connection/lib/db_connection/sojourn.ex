defmodule DBConnection.SojournError do
  defexception [:message]

  def exception(message), do: %DBConnection.SojournError{message: message}
end

defmodule DBConnection.Sojourn do
  @moduledoc """
  A `DBConnection.Pool` using sbroker.

  ### Options

    * `:pool_size` - The number of connections (default: `10`)
    * `:pool_overflow` - The number of extra connections that can be created if
    required (default: `0`)
    * `:broker` - The `:sbroker` callback module (see `:sbroker`,
    default: `DBConnection.Sojourn.Broker`)
    * `:broker_start_opt` - Start options for the broker (see
    `:sbroker`, default: `[]`)
    * `:regulator` - The `:sregulator` callback module (see `:sregulator`,
    default: `DBConnection.Sojourn.Regulator`)
    * `:regulator_start_opt` - Start options for the regulator (see
    `:sregulator`, default; `[]`)
    * `:max_restarts` - the maximum amount of connection restarts allowed in a
    time frame (default `3`)
    * `:max_seconds` - the time frame in which `:max_restarts` applies (default
    `5`)
    * `:shutdown` - the shutdown strategy for connections (default `5_000`)

  All options are passed as the argument to the sbroker callback module. This
  pool overrides `:idle` to always be `:passive` and may not honour
  `:idle_timeout` if it tries to prevent the connection queue becoming too short
  or spreads out pings evenly.
  """

  @behaviour  DBConnection.Pool

  @broker    DBConnection.Sojourn.Broker
  @regulator DBConnection.Sojourn.Regulator
  @timeout   15_000

  import Supervisor.Spec

  @doc false
  def ensure_all_started(_opts, type) do
    Application.ensure_all_started(:sbroker, type)
  end

  @doc false
  def start_link(mod, opts) do
    apply(:sbroker, :start_link, broker_args(mod, opts))
  end

  @doc false
  def child_spec(mod, opts, child_opts \\ []) do
    worker(:sbroker, broker_args(mod, opts), child_opts ++ [modules: :dynamic])
  end

  @doc false
  def checkout(broker, opts) do
    case ask(broker, opts) do
      {:go, ref, {pid, mod, state}, _, _} ->
        {:ok, {pid, ref}, mod, state}
      :drop ->
        message = "connection not available and queuing is disabled"
        {:error, DBConnection.ConnectionError.exception(message)}
      {:drop, wait} ->
        wait = :erlang.convert_time_unit(wait, :native, :milli_seconds)
        message = "connection not available " <>
        "and request was dropped from queue after #{wait}ms"
        {:error, DBConnection.ConnectionError.exception(message)}
    end
  end

  @doc false
  defdelegate checkin(ref, state, opts), to: DBConnection.Connection

  @doc false
  defdelegate disconnect(ref, err, state, opts), to: DBConnection.Connection

  @doc false
  defdelegate stop(ref, err, state, opts), to: DBConnection.Connection

  ## Helpers

  defp broker_args(mod, opts) do
    broker     = Keyword.get(opts, :broker, @broker)
    start_opts = Keyword.get(opts, :broker_start_opt, [])
    args       = [__MODULE__.Broker, {broker, mod, opts}, start_opts]
    case Keyword.get(opts, :name) do
      nil                     -> args
      name when is_atom(name) -> [{:local, name} | args]
      name                    -> [name | args]
    end
  end

  defp ask(broker, opts) do
    timeout = Keyword.get(opts, :timeout, @timeout)
    info = {self(), timeout}
    broker = via(broker, opts)
    case Keyword.get(opts, :queue, true) do
      true  -> :sbroker.ask(broker, info)
      false -> nb_ask(broker, info)
    end
  end

  defp via(broker, opts) do
    case Keyword.get(opts, :protector, true) do
      true  -> {:via, :sprotector, {broker, :ask}}
      false -> broker
    end
  end

  defp nb_ask(broker, info) do
    case :sbroker.nb_ask(broker, info) do
      {:go, _, _, _, _} = go -> go
      {:drop, _}             -> :drop
    end
  end
end
