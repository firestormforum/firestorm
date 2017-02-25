defmodule FirestormWeb.ThreadView do
  use FirestormWeb.Web, :view
  alias FirestormData.{Thread}

  def user(thread) do
    {:ok, user} = Thread.user(thread)
    user
  end

  def markdown(body) do
    body
    |> Earmark.as_html!
    |> raw
  end

  def back("show.html", conn) do
    category = conn.assigns[:category]
    category_path(conn, :show, category.slug)
  end
  def back(_template, _conn), do: nil
end
