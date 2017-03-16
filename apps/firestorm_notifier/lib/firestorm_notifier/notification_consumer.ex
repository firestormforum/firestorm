defmodule FirestormNotifier.NotificationConsumer do
  use GenStage
  alias FirestormNotifier.{Emails, Mailer}
  alias FirestormData.{Followable, Repo, User}

  def start_link() do
    GenStage.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    {:consumer, :the_state_does_not_matter, subscribe_to: [FirestormData.Events]}
  end

  def handle_events(events, _from, state) do
    for event <- events, do: handle_event(event)

    # We are a consumer, so we would never emit items.
    {:noreply, [], state}
  end

  def handle_event({:new_post, post}) do
    IO.puts "there was a new post! #{inspect post}"
    notify_thread_followers(post)
  end
  # If we don't care about the event, do nothing
  def handle_event(_), do: :ok

  defp notify_thread_followers(post) do
    thread = post.thread
    for user_id <- Followable.follower_ids(thread) do
      user = Repo.get(User, user_id)
      if user.email do
        if(post.user.id !== user.id) do
          mail = Emails.thread_new_post_notification(user, thread, post)
          Mailer.deliver_now(mail)
        end
      end
    end
  end
end
