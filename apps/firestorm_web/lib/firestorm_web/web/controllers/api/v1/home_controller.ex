defmodule FirestormWeb.Web.Api.V1.HomeController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.GetHomeCategories

  def index(conn, _params) do
    {:ok, categories} = GetHomeCategories.run(%GetHomeCategories{user_id: 1})

    conn
    |> render("index.json", categories: categories)
  end
end
