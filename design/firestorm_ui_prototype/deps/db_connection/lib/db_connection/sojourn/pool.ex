defmodule DBConnection.Sojourn.Pool do
  @moduledoc false

  @regulator DBConnection.Sojourn.Regulator

  import Supervisor.Spec

  def start_link(owner, mod, opts) do
    children   = [regulator(opts), conn_sup(mod, opts), starter(owner, opts),
                  watcher(owner)]
    opts       = [strategy: :one_for_all, max_restarts: 0]
    Supervisor.start_link(children, opts)
  end

  def lookup_regulator(sup) do
    children = Supervisor.which_children(sup)
    {_, regulator, _, _}  = List.keyfind(children, :regulator, 0)
    regulator
  end

  def lookup_conn_sup(sup) do
    children = Supervisor.which_children(sup)
    {_, conn_sup, _, _}  = List.keyfind(children, :conn_sup, 0)
    conn_sup
  end

  ## Helpers

  defp watcher(owner) do
    worker(DBConnection.Watcher, [owner])
  end

  defp regulator(opts) do
    regulator = Keyword.get(opts, :regulator, @regulator)
    start_opts = Keyword.get(opts, :regulator_start_opt, [])
    child_opts = [id: :regulator, modules: :dynamic]
    worker(:sregulator, [regulator, opts, start_opts], child_opts)
  end

  defp conn_sup(mod, opts) do
    child_opts = Keyword.take(opts, [:shutdown])
    conn = DBConnection.Connection.child_spec(mod, opts, :sojourn, child_opts)
    sup_opts  = Keyword.take(opts, [:max_restarts, :max_seconds])
    sup_opts  = [strategy: :simple_one_for_one] ++ sup_opts
    supervisor(Supervisor, [[conn], sup_opts], [id: :conn_sup])
  end

  defp starter(owner, opts) do
    worker(DBConnection.Sojourn.Starter, [owner, opts], [restart: :transient])
  end
end
