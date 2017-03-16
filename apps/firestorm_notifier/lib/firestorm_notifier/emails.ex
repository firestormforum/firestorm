defmodule FirestormNotifier.Emails do
  import Bamboo.Email
  alias FirestormWeb.Web.Endpoint
  alias FirestormWeb.Markdown
  import FirestormWeb.Web.Router.Helpers
  import FirestormWeb.Web.SlugHelpers
  import Phoenix.HTML
  import Phoenix.HTML.Link

  def thread_new_post_notification(user, thread, post) do
    new_email
    |> to(user.email)
    |> from("noreply@firestormforum.org")
    |> subject("There was a new post in a thread you are watching on Firestorm")
    |> html_body(html_body_for(thread, post))
    |> text_body(text_body_for(thread, post))
    |> put_header("reply-to", reply_to_thread_address(thread))
  end

  defp reply_to_thread_address(%{id: id}) do
    "reply-thread-#{id}@#{inbound_email_domain}"
  end

  defp inbound_email_domain do
    # This goes into config at some point
    "notifier.firestormforum.org"
  end

  defp html_body_for(thread, post) do
    """
    <p>The Firestorm thread #{link(thread.title, to: category_thread_url(Endpoint, :show, category_finder(thread.category), thread.id)) |> safe_to_string} has received a reply:</p>
    <hr />
    #{Markdown.render(post.body)}
    """
  end

  defp text_body_for(thread, post) do
    """
    The Firestorm thread #{link(thread.title, to: category_thread_url(Endpoint, :show, category_finder(thread.category), thread.id)) |> safe_to_string} has received a reply:\n\n
    #{post.body}
    """
  end
end
