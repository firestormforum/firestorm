defmodule FirestormWeb.ThreadController do
  use FirestormWeb.Web, :controller

  def show(conn, %{"id" => id}) do
    query =
      from t in Thread,
      where: t.id == ^id,
      preload: [:posts]

    thread = query |> Repo.one

    render conn, "show.html", thread: thread
  end
end
