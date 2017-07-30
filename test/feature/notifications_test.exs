defmodule FirestormWeb.Feature.NotificationsTest do
  use FirestormWeb.Web.FeatureCase, async: false
  alias FirestormWeb.Forums

  @bob %{email: "bob@example.com", name: "Bob Belcher", username: "bob"}
  @fred %{email: "fred@example.com", name: "Fred Bankston", username: "fred"}
  @elixir %{title: "Elixir"}
  @otp_is_cool %{title: "OTP is cool", body: "Don't you think?"}
  @genstage %{title: "GenStage", body: "Kinda streamy no?"}

  setup do
    {:ok, _} = FirestormWeb.Notifications.start_link()
    {:ok, bob} = Forums.create_user(@bob)
    {:ok, fred} = Forums.create_user(@fred)
    {:ok, elixir} = Forums.create_category(@elixir)
    {:ok, _otp_is_cool} = Forums.create_thread(elixir, bob, @otp_is_cool)
    {:ok, genstage} = Forums.create_thread(elixir, bob, @genstage)
    {:ok, _watch} = Forums.watch(bob, genstage)
    {:ok, _post} = Forums.create_post(genstage, fred, %{body: "Herp Derp"})

    {:ok, bob: bob, thread: genstage}
  end

  test "viewing notifications", %{session: session, bob: bob} do
    import Page.Notification.Index

    session
    |> log_in_as(bob)
    |> visit(notification_path(FirestormWeb.Web.Endpoint, :index))
    |> assert_has(notifications(1))
  end

  test "viewing a single notification", %{session: session, bob: bob} do
    import Page.Notification.{Index, Show}

    session =
      session
      |> log_in_as(bob)
      |> visit(notification_path(FirestormWeb.Web.Endpoint, :index))

    session
    |> all(notifications(1))
    |> List.first()
    |> click(notification_link())

    session
    |> assert_has(notification_body())
    |> assert_has(notification_view_button())
  end
end
