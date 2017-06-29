defmodule FirestormWeb.Feature.CategoriesTest do
  use FirestormWeb.Web.FeatureCase, async: true
  alias FirestormWeb.Forums
  alias FirestormWeb.Web.Endpoint

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

  test "category show shows posts count for threads", %{session: session} do
    import Page.Category.Show
    {:ok, [elixir, _elm]} = create_categories(["Elixir", "Elm"])
    {:ok, user} = Forums.create_user(%{username: "knewter", email: "josh@dailydrip.com", name: "Josh Adams"})
    otp_is_cool_parameters = %{title: "OTP is cool", body: "Don't you think?"}
    {:ok, otp_is_cool} = Forums.create_thread(elixir, user, otp_is_cool_parameters)
    {:ok, _post} = Forums.create_post(otp_is_cool, user, %{body: "Yup"})

    session
    |> visit(category_path(Endpoint, :show, elixir.id))
    |> all(threads(1))
    |> List.first()
    |> assert_has(thread_title("OTP is cool"))
    |> assert_has(thread_posts_count(2, unread: true))
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
