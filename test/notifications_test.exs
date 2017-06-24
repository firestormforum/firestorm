defmodule FirestormWeb.NotificationsTest do
  use FirestormWeb.DataCase
  alias FirestormWeb.Forums
  use Bamboo.Test, shared: true

  setup do
    {:ok, _} = FirestormWeb.Notifications.start_link()

    :ok
  end

  test "creating a post in a thread notifies everyone watching the thread" do
    {:ok, user} = Forums.create_user(%{username: "knewter", email: "josh@dailydrip.com", name: "Josh Adams"})
    {:ok, bob} = Forums.create_user(%{username: "bob", email: "bob@bob.com", name: "Bob Vladbob"})
    {:ok, elixir} = Forums.create_category(%{title: "Elixir"})
    {:ok, otp_is_cool} = Forums.create_thread(elixir, user, %{title: "OTP is cool", body: "Don't you think?"})
    {:ok, _} = user |> Forums.watch(otp_is_cool)
    {:ok, yup} = Forums.create_post(otp_is_cool, bob, %{body: "yup"})
    assert_delivered_email FirestormWeb.Emails.thread_new_post_notification(user, otp_is_cool, yup)
    refute_delivered_email FirestormWeb.Emails.thread_new_post_notification(bob, otp_is_cool, yup)
  end
end
