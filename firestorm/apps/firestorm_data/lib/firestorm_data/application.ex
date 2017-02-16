defmodule FirestormData.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(FirestormData.Repo, [])
    ]

    opts = [strategy: :one_for_one, name: FirestormData.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
