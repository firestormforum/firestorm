defmodule FirestormWeb.Web.InboundController do
  use FirestormWeb.Web, :controller
  require Logger
  alias FirestormData.Commands.CreatePost
  # NOTE: We should get rid of this and add a query to find users by email
  alias FirestormData.Repo

  @inbound_domain "notifier.firestormforum.org"
  # Get the thread id out of something like
  # "replythread-123@notifier.firestormforum.org"
  @thread_email_regex ~r/reply-thread-([0-9]*)@#{@inbound_domain}/
  # Get the email part out of something like "Josh Adams <josh@dailydrip.com>"
  @email_regex ~r/[^<]*<?(.*@[^>]*)[^>]*/

  def sendgrid(conn, params) do
    body = params["text"]
    from_email_param = params["from"]
    to_email = params["to"]

    with [_,from_email|_] <- Regex.run(@email_regex, from_email_param),
         [_,thread_id|_] <- Regex.run(@thread_email_regex, to_email),
         user when not is_nil(user) <-  Repo.get_by(User, email: from_email),
         {:ok, _} <- create_post(user.id, thread_id, body) do
      conn
      |> render("sendgrid.json", %{})
    else
      _ ->
        conn
        |> render("sendgrid.json", %{})
    end
  end

  defp create_post(user_id, thread_id, body) do
    %CreatePost{}
    |> CreatePost.changeset(%{user_id: user_id, thread_id: thread_id, body: body})
    |> CreatePost.run
  end
end
