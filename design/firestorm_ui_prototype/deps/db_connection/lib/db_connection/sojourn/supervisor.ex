defmodule DBConnection.Sojourn.Supervisor do
  @moduledoc false

  import Supervisor.Spec

  def start_link() do
    child_mod = DBConnection.Sojourn.Pool
    children  = [supervisor(child_mod, [], [restart: :temporary])]
    opts      = [strategy: :simple_one_for_one, name: __MODULE__]
    Supervisor.start_link(children, opts)
  end

  def start_pool(mod, opts) do
    Supervisor.start_child(__MODULE__, [self(), mod, opts])
  end
end
