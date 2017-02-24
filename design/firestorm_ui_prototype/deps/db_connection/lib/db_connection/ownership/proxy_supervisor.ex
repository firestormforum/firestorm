defmodule DBConnection.Ownership.ProxySupervisor do
  @moduledoc false
  import Supervisor.Spec

  def start_link do
    children  = [supervisor(DBConnection.Ownership.Proxy, [], [restart: :temporary])]
    opts      = [strategy: :simple_one_for_one]
    Supervisor.start_link(children, opts)
  end

  def start_owner(sup, from, pool, opts) do
    Supervisor.start_child(sup, [self(), from, pool, opts])
  end
end