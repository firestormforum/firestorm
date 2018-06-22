defmodule FirestormWeb.Web.InboundController do
  use FirestormWeb.Web, :controller
  alias FirestormWeb.Forums

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

    with [_, from_email | _] <- Regex.run(@email_regex, from_email_param),
         [_, thread_id | _] <- Regex.run(@thread_email_regex, to_email),
         thread <- Forums.get_thread(thread_id),
         user when not is_nil(user) <- Forums.get_user_by_email(from_email),
         {:ok, _} <- Forums.create_post(thread, user, %{body: body}) do
      # FIXME: Post created successfully, maybe I want to instrument this
      # in the future.
    else
      _ ->
        :ok
        # FIXME: Maybe I want to report failures to a logging service here.
    end

    send_resp(conn, 200, "ok")
  end
end
