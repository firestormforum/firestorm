defmodule FirestormWeb.Application do
  @moduledoc """
  The FirestormWeb.Application module
  """

  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(FirestormWeb.Web.Endpoint, []),
    ]

    opts = [strategy: :one_for_one, name: FirestormWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
