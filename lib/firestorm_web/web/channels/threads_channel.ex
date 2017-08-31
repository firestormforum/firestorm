defmodule FirestormWeb.Web.ThreadsChannel do
  use FirestormWeb.Web, :channel
  use Appsignal.Instrumentation.Decorators
  alias FirestormWeb.Store.{ReplenishResponse, ReplenishRequest}
  alias FirestormWeb.Web.Api.V1.FetchView
  alias FirestormWeb.{Repo, Forums}

  intercept ["update"]

  def join("threads:" <> _id, payload, socket) do
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  defp authorized?(_) do
    true
  end

  @decorate channel_action()
  def handle_out("update", msg, socket) do
    push socket, "update", FetchView.render("index.json", msg)
    {:noreply, socket}
  end
end
