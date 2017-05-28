defmodule FirestormWeb.Feature.CategoriesTest do
  use FirestormWeb.Web.FeatureCase, async: true
  alias FirestormWeb.Forums

  test "creating a new category", %{session: session} do
    import Page.Category.{New, Index}
    alias Page.Category.Show

    session
    |> visit("/")
    |> click(new_category_link())
    |> fill_in(title_field(), with: "Erlang")
    |> click(create_category_button())
    |> assert_has(Show.category_title("Erlang"))
  end

  test "categories are listed", %{session: session} do
    import Page.Category.Index
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
