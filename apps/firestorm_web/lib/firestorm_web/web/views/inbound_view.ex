defmodule FirestormWeb.Web.InboundView do
  use FirestormWeb.Web, :view

  def render("sendgrid.json", %{}) do
    "ok"
  end
end
