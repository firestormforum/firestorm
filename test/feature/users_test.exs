defmodule FirestormWeb.Feature.UsersTest do
  use FirestormWeb.Web.FeatureCase, async: true
  alias FirestormWeb.Forums

  @bob %{email: "bob@example.com", name: "Bob Belcher", username: "bob"}

  setup do
    {:ok, bob} = Forums.create_user(@bob)
    {:ok, bob: bob}
  end

  test "viewing a user", %{session: session, bob: bob} do
    import Page.User.Show

    session
    |> visit(user_path(FirestormWeb.Web.Endpoint, :show, bob))
    |> assert_has(user_details())
    |> assert_has(user_posts())
    |> Browser.take_screenshot()
  end
end
