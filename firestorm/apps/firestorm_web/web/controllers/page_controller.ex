defmodule FirestormWeb.PageController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.GetHomeCategories

  def index(conn, _params) do
    case logged_in?(conn) do
      true ->
        conn
        |> redirect(to: page_path(conn, :home))
      false ->
        conn
        |> render("index.html")
    end
  end

  def home(conn, _params) do
    {:ok, categories} = GetHomeCategories.run(%GetHomeCategories{ user_id: 1 })

    conn
    |> render("home.html", categories: categories)
  end

  def thread(conn, _params) do
    conn
    |> render("thread.html")
  end
end
