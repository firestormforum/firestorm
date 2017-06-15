defmodule FirestormWeb.NotificationsTest do
  use FirestormWeb.DataCase
  alias FirestormWeb.Forums
  use Bamboo.Test, shared: true

  test "creating a post in a thread notifies everyone involved in the thread" do
    # TODO: This isn't actually what we want to do, so fix this later.
    {:ok, user} = Forums.create_user(%{username: "knewter", email: "josh@dailydrip.com", name: "Josh Adams"})
    {:ok, elixir} = Forums.create_category(%{title: "Elixir"})
    {:ok, otp_is_cool} = Forums.create_thread(elixir, user, %{title: "OTP is cool", body: "Don't you think?"})
    {:ok, yup} = Forums.create_post(otp_is_cool, user, %{body: "yup"})
    assert_delivered_email FirestormWeb.Emails.thread_new_post_notification(user, otp_is_cool, yup)
  end
end
