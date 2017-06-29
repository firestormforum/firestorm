defmodule FirestormWeb.Feature.ThreadsTest do
  use FirestormWeb.Web.FeatureCase, async: true
  alias FirestormWeb.Forums

  @otp_is_cool_parameters %{title: "OTP is cool", body: "Don't you think?"}

  test "creating a new thread", %{session: session} do
    import Page.Thread.{New, Show}
    import Page.Category.Show, only: [new_thread_link: 0]
    {:ok, [elixir]} = create_categories(["Elixir"])
    {:ok, user} = Forums.create_user(%{username: "knewter", email: "josh@dailydrip.com", name: "Josh Adams"})

    session
    |> log_in_as(user)
    |> visit(category_path(FirestormWeb.Web.Endpoint, :show, elixir))
    |> click(new_thread_link())
    |> fill_in(title_field(), with: @otp_is_cool_parameters[:title])
    |> fill_in(body_field(), with: @otp_is_cool_parameters[:body])
    |> click(create_thread_button())
    |> assert_has(thread_title(@otp_is_cool_parameters[:title]))
  end

  test "creating a new thread when unauthenticated", %{session: session} do
    import Page.Layout
    import Page.Category.Show
    {:ok, [elixir]} = create_categories(["Elixir"])

    session
    |> visit(category_path(FirestormWeb.Web.Endpoint, :show, elixir))
    |> click(new_thread_link())
    |> assert_has(error("You must be logged in to access this page."))
  end

  test "replying to a thread", %{session: session} do
    import Page.Thread.Show
    import Page.Post.New

    {:ok, [elixir]} = create_categories(["Elixir"])
    {:ok, user} = Forums.create_user(%{username: "knewter", email: "josh@dailydrip.com", name: "Josh Adams"})
    {:ok, otp_is_cool} = Forums.create_thread(elixir, user, @otp_is_cool_parameters)

    session
    |> log_in_as(user)
    |> visit(category_thread_path(FirestormWeb.Web.Endpoint, :show, elixir, otp_is_cool))
    |> assert_has(thread_title(@otp_is_cool_parameters[:title]))
    |> click(reply_link())
    |> fill_in(body_field(), with: "I agree!")
    |> click(reply_button())
    |> assert_has(thread_title(@otp_is_cool_parameters[:title]))
    |> assert_has(post_item("I agree!"))
    |> assert_has(post_username("knewter", 2))
  end

  @tag :pending
  test "watching a thread", %{session: session} do
    import Page.Thread.Show

    {:ok, [elixir]} = create_categories(["Elixir"])
    {:ok, user} = Forums.create_user(%{username: "knewter", email: "josh@dailydrip.com", name: "Josh Adams"})
    {:ok, otp_is_cool} = Forums.create_thread(elixir, user, @otp_is_cool_parameters)

    session
    |> log_in_as(user)
    |> visit(category_thread_path(FirestormWeb.Web.Endpoint, :show, elixir, otp_is_cool))
    |> refute_has(watched_icon())
    |> click(watch_link())
    |> assert_has(watched_icon())
    |> click(watch_link())
    |> refute_has(watched_icon())
  end

  defp create_categories(titles) do
    categories =
      for title <- titles do
        {:ok, category} = Forums.create_category(%{title: title})
        category
      end
    {:ok, categories}
  end

  defp log_in_as(session, user) do
    session
    |> visit("/")
    |> Browser.set_cookie("current_user", user.id)
  end
end
