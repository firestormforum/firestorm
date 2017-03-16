defmodule FirestormWeb.Web.Api.V1.CategoryController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.GetCategory
  alias FirestormWeb.Web.Api.V1.HomeView

  def show(conn, %{"id" => id_or_slug}) do
    finder = get_finder(id_or_slug)

    case GetCategory.run(%GetCategory{finder: finder}) do
      {:ok, c} ->
        conn
        |> render(HomeView, "index.json", categories: [c])
    end
  end
end
