defmodule FirestormWeb.Feature.CategoriesTest do
  use FirestormWeb.Web.FeatureCase, async: true
  alias FirestormWeb.Forums

  defmodule Page.CategoriesIndex do
    import Wallaby.Query

    def categories(count), do: css("ol.category-list li", count: count)
    def category_title(text), do: css("h2.title", text: text)
  end

  defmodule Page.CategoriesNew do
    import Wallaby.Query

    def title_field(), do: text_field("Title")
    def create_category_button(), do: button("Create Category")
  end

  defmodule Page.Home do
    import Wallaby.Query

    def new_category_link(), do: link("New Category")
  end

  defmodule Page.CategoriesShow do
    import Wallaby.Query

    def category_title(title), do: css("h2 a", text: title)
  end

  test "creating a new category", %{session: session} do
    import Page.CategoriesNew
    import Page.Home
    import Page.CategoriesShow

    session
    |> visit("/")
    |> click(new_category_link())
    |> fill_in(title_field(), with: "Erlang")
    |> click(create_category_button())
    |> assert_has(category_title("Erlang"))
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
