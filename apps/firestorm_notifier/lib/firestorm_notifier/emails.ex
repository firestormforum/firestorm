defmodule FirestormNotifier.Emails do
  import Bamboo.Email
  import FirestormWeb.Web.Router.Helpers

  def thread_new_post_notification(user, thread, post) do
    new_email
    |> to(user.email)
    |> from("noreply@firestormforum.org")
    |> subject("There was a new post in a thread you are watching on Firestorm")
    |> html_body("thread: #{thread.title}")
    |> text_body("thread: #{thread.title}")
  end
end
