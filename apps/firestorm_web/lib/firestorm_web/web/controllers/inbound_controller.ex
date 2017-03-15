defmodule FirestormWeb.Web.InboundController do
  use FirestormWeb.Web, :controller
  require Logger

  def sendgrid(conn, params) do
    Logger.warn(inspect(params))

    conn
    |> render("sendgrid.json", %{})
  end
end
