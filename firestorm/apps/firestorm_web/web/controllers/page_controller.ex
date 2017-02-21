defmodule FirestormWeb.PageController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.GetHomeCategories

  def index(conn, _params) do
    case logged_in?(conn) do
      true ->
        redirect conn, to: page_path(conn, :home)
      false ->
        render conn, "index.html"
    end
  end

  def home(conn, _params) do
    {:ok, categories} = GetHomeCategories.run(%GetHomeCategories{ user_id: 1 })

    render conn, "home.html", categories: categories
  end

  def thread(conn, _params) do
    render conn, "thread.html"
  end
end
