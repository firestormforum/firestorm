defmodule FirestormWeb.Web.Api.V1.PreviewView do
  use FirestormWeb.Web, :view

  def render("show.json", %{html: html}) do
    %{data: %{html: html}}
  end
end
