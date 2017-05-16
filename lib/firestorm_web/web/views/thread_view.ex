defmodule FirestormWeb.Web.ThreadView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Markdown

  def markdown(body) do
    body
    |> Markdown.render
    |> raw
  end
end
