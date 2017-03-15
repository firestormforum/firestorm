defmodule FirestormWeb.Web.Api.V1.HomeView do
  use FirestormWeb.Web, :view

  def render("index.json", %{categories: categories}) do
    %{
      categories: Enum.map(categories, &category_json/1)
    }
  end

  def category_json(category) do
    %{
      id: category.id,
      title: category.title,
      inserted_at: category.inserted_at,
      updated_at: category.updated_at,
    }
  end
end
