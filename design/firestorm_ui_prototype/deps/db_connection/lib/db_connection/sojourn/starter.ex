defmodule DBConnection.Sojourn.Starter do
  @moduledoc false

  use Connection

  def start_link(broker, opts) do
    Connection.start_link(__MODULE__, {self(), broker, opts})
  end

  def init(args), do: {:connect, :init, args}

  def connect(:init, {sup, broker, opts}) do
    size = Keyword.get(opts, :pool_size, 10)
    overflow = Keyword.get(opts, :pool_overflow, 0)
    regulator = DBConnection.Sojourn.Pool.lookup_regulator(sup)
    conn_sup = DBConnection.Sojourn.Pool.lookup_conn_sup(sup)
    %{workers: conns} = Supervisor.count_children(conn_sup)
    start_conns(size + overflow - conns, conn_sup, {broker, regulator})
  end

  ## Helpers

  defp start_conns(n, conn_sup, info) when n > 0 do
    case Supervisor.start_child(conn_sup, [info]) do
      {:ok, _} ->
        start_conns(n - 1, conn_sup, info)
      {:error, reason} ->
        {:stop, {:failed_to_start_connection, reason}, info}
    end
  end
  defp start_conns(_, _, _), do: {:stop, :normal, nil}
end
