defmodule FirestormWeb.ThreadView do
  use FirestormWeb.Web, :view
  alias FirestormData.{Thread}

  def user(thread) do
    {:ok, user} = Thread.user(thread)
    user
  end

  def markdown(body) do
    body
    |> Earmark.to_html
    |> raw
  end
end
