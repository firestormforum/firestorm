defmodule FirestormWeb.Emails do
  @moduledoc """
  Module hosting outbound email templates.
  """

  import Bamboo.Email
  import FirestormWeb.Web.Router.Helpers, only: [category_thread_url: 4]
  import Phoenix.HTML
  import Phoenix.HTML.Link
  alias FirestormWeb.Web.Endpoint
  alias FirestormWeb.Markdown
  alias FirestormWeb.Forums.{Post, Thread, User}

  def thread_new_post_notification(%User{} = user, %Thread{} = thread, %Post{} = post) do
    new_email()
    |> to(user.email)
    |> from("noreply@firestormforum.org")
    |> subject("There was a new post in a thread you are watching on Firestorm")
    |> html_body(html_body_for(thread, post))
    |> text_body(text_body_for(thread, post))
    |> put_header("reply-to", reply_to_thread_address(thread))
  end

  defp reply_to_thread_address(%{id: id}) do
    "reply-thread-#{id}@#{inbound_email_domain()}"
  end

  defp inbound_email_domain do
    # This goes into config at some point
    "notifier.firestormforum.org"
  end

  def thread_url(thread) do
    category_thread_url(Endpoint, :show, thread.category_id, thread.id)
  end

  def post_url(thread, post) do
    "#{thread_url(thread)}#post-#{post.id}"
  end

  defp html_body_for(%Thread{} = thread, %Post{} = post) do
    """
    <p>The Firestorm thread #{thread.title |> link(to: thread_url(thread)) |> safe_to_string} has received #{"a reply" |> link(to: post_url(thread, post)) |> safe_to_string}:</p>
    <hr />
    #{Markdown.render(post.body)}
    """
  end

  defp text_body_for(%Thread{} = thread, %Post{} = post) do
    """
    The Firestorm thread #{thread.title} [#{thread_url(thread)}] has received a reply [#{post_url(thread, post)}]:\n\n
    #{post.body}
    """
  end
end
