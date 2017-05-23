defmodule FirestormWeb.Feature.CategoriesTest do
  use FirestormWeb.Web.FeatureCase, async: true
  alias FirestormWeb.Forums

  defmodule Page.CategoriesIndex do
    import Wallaby.Query

    def categories(count), do: css("ol.category-list li", count: count)
    def category_title(text), do: css("h2.title", text: text)
  end

  test "categories are listed", %{session: session} do
    import Page.CategoriesIndex
    {:ok, [_elixir, _elm]} = create_categories(["Elixir", "Elm"])

    session
    |> visit("/")
    |> find(categories(2))
    |> List.first()
    |> assert_has(category_title("Elixir"))
  end

  def create_categories(titles) do
    categories =
      for title <- titles do
        {:ok, category} = Forums.create_category(%{title: title})
        category
      end
    {:ok, categories}
  end
end
