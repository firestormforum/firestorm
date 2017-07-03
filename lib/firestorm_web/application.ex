defmodule FirestormWeb.Application do
  @moduledoc false

  use Application
  import Supervisor.Spec

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: FirestormWeb.Supervisor]
    Supervisor.start_link(children(Mix.env), opts)
  end

  defp default_children() do
    [
      # Start the Ecto repository
      supervisor(FirestormWeb.Repo, []),
      # Start the endpoint when the application starts
      supervisor(FirestormWeb.Web.Endpoint, [])
    ]
  end
  defp children(:test) do
    default_children()
  end
  defp children(_) do
    default_children() ++
    [
      # Start the notifications server
      worker(FirestormWeb.Notifications, [])
    ]
  end
end
