defmodule FirestormWeb.PageController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.GetHomeCategories

  def index(conn, _params) do
    render conn, "index.html"
  end

  def home(conn, _params) do
    {:ok, categories} = GetHomeCategories.run(%GetHomeCategories{ user_id: 1 })

    render conn, "home.html", categories: categories
  end

  def thread(conn, _params) do
    render conn, "thread.html"
  end
end
