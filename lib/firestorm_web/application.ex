defmodule FirestormWeb.Application do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec

    # Define workers and child supervisors to be supervised
    children = [
      # Start the Ecto repository
      supervisor(FirestormWeb.Repo, []),
      # Start the endpoint when the application starts
      supervisor(FirestormWeb.Web.Endpoint, []),
      # Start the notifications server
      worker(FirestormWeb.Notifications, [])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: FirestormWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
