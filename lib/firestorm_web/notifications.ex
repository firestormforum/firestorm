defmodule FirestormWeb.Notifications do
  @moduledoc """
  This is the entry point for our Notifications system. It will grow to support
  various notification mechanisms, but for now it will send an email to anyone
  involved in a thread everytime there's a new post.
  """

  use GenServer
  alias FirestormWeb.Forums
  alias FirestormWeb.Forums.{Post, Notification}
  alias FirestormWeb.{Repo, Emails, Mailer}
  alias FirestormWeb.Web.Endpoint

  ### Client API
  @doc """
  Start the notifications system and link it.

      iex> {:ok, pid} = FirestormWeb.Notifications.start_link()

  """
  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Send a notification that a new post has been made.

      iex> :ok = FirestormWeb.Notifications.post_created(%Post{})

  """
  def post_created(%Post{} = post) do
    GenServer.cast(__MODULE__, {:post_created, post})
  end

  ### Server API
  def init(_) do
    {:ok, :nostate}
  end

  def handle_cast({:post_created, %Post{} = post}, :nostate) do
    # 1) Find all users that are watching this thread
    with post when not is_nil(post) <- Repo.preload(post, [thread: [:watchers]]),
         thread when not is_nil(thread) <- post.thread,
         users <- thread.watchers |> Enum.uniq do
           # 2) Send each of them an email about it and notify them via the
           # internal notifications system.
           for user <- users do
             # Send internal notification
             {:ok, _} = Forums.notify(user, Notification.thread_new_post_notification(post.thread, post))

             # Send email
             user
             |> Emails.thread_new_post_notification(post.thread, post)
             |> Mailer.deliver_now()
           end
    else
      _ ->
        :ok
    end
    {:noreply, :nostate}
  end
end
