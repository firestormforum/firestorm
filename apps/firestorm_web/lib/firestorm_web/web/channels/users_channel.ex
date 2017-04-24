defmodule FirestormWeb.Web.UsersChannel do
  use FirestormWeb.Web, :channel
  alias FirestormWeb.Web.Api.V1.{
    FetchView
  }
  import Ecto.Query

  intercept ["update"]

  def join("users:" <> id, payload, socket) do
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end

  def handle_out("update", msg, socket) do
    push socket, "update", FetchView.render("index.json", msg)
  end
end
