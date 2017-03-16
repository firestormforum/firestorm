defmodule FirestormNotifier.EmailsTest do
  use ExUnit.Case
  import Bamboo.Email
  alias FirestormNotifier.Emails

  test "thread new post notifications have appropriate reply-to" do
    user = %{email: "bob@example.com"}
    thread = %{title: "Some thread", id: "123", category: %{id: 1, slug: "foo"}}
    post = %{body: "Foo"}
    email = Emails.thread_new_post_notification(user, thread, post)
    assert email.headers["reply-to"] == "reply-thread-123@notifier.firestormforum.org"
  end
end
