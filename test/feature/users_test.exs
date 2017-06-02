defmodule FirestormWeb.Feature.UsersTest do
  use FirestormWeb.Web.FeatureCase, async: true
  alias FirestormWeb.Forums

  @bob %{email: "bob@example.com", name: "Bob Belcher", username: "bob"}
  @elixir %{title: "Elixir"}
  @otp_is_cool %{title: "OTP is cool", body: "Don't you think?"}
  @genstage %{title: "GenStage", body: "Kinda streamy no?"}

  setup do
    {:ok, bob} = Forums.create_user(@bob)
    {:ok, elixir} = Forums.create_category(@elixir)
    {:ok, _otp_is_cool} = Forums.create_thread(elixir, bob, @otp_is_cool)
    {:ok, _genstage} = Forums.create_thread(elixir, bob, @genstage)

    {:ok, bob: bob}
  end

  test "viewing a user", %{session: session, bob: bob} do
    import Page.User.Show

    session
    |> visit(user_path(FirestormWeb.Web.Endpoint, :show, bob))
    |> assert_has(user_details())
    |> assert_has(user_posts(2))
    |> Browser.take_screenshot()
  end
end
