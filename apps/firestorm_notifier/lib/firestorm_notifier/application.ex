defmodule FirestormNotifier.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(FirestormNotifier.NotificationConsumer, [])
    ]

    opts = [strategy: :one_for_one, name: FirestormNotifier.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
