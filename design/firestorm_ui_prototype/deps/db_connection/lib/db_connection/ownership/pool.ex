defmodule DBConnection.Ownership.Pool do
  @moduledoc false

  import Supervisor.Spec

  def start_link(owner, mod, opts) do
    children = [watcher(owner),
                DBConnection.child_spec(mod, opts, [id: :pool]),
                supervisor(DBConnection.Ownership.ProxySupervisor, [],
                  [id: :owner_sup])]
    sup_opts = [strategy: :rest_for_one, max_restarts: 0]
    Supervisor.start_link(children, sup_opts)
  end

  def pool_pid(pool) do
    fetch_pid(pool, :pool)
  end

  def owner_sup_pid(pool) do
    fetch_pid(pool, :owner_sup)
  end

  ## Helpers

  defp fetch_pid(pool, key) do
    children = Supervisor.which_children(pool)
    case List.keyfind(children, key, 0) do
      {_, pool, _, _} when is_pid(pool) ->
        pool
      _ ->
        raise "#{key} is not running"
    end
  end

  defp watcher(owner) do
    worker(DBConnection.Watcher, [owner])
  end
end
