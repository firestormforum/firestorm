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
    |> put_header("Reply-To", reply_to_thread_address(thread))
  end

  defp reply_to_thread_address(%{id: id}) do
    "reply-thread-#{id}@#{inbound_email_domain}"
  end

  defp inbound_email_domain do
    # This goes into config at some point
    "notifier.firestormforum.org"
  end
end
