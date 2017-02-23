defmodule FirestormWeb do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(FirestormWeb.Endpoint, []),
    ]

    opts = [strategy: :one_for_one, name: FirestormWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def config_change(changed, _new, removed) do
    FirestormWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
