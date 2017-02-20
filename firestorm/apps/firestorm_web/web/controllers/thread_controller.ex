defmodule FirestormWeb.ThreadController do
  use FirestormWeb.Web, :controller
  alias FirestormData.{Category, Post, Thread, User, Repo}
  import Ecto.Query, only: [from: 2]

  def show(conn, %{"id" => id}) do
    query =
      from t in Thread,
      where: t.id == ^id,
      preload: [:posts]

    thread = query |> Repo.one

    render conn, "show.html", thread: thread
  end
end
