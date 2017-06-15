defmodule FirestormWeb.Notifications do
  @moduledoc """
  This is the entry point for our Notifications system. It will grow to support
  various notification mechanisms, but for now it will send an email to anyone
  involved in a thread everytime there's a new post.
  """

  use GenServer
  alias FirestormWeb.Forums.Post
  alias FirestormWeb.{Repo, Emails, Mailer}

  ### Client API
  @moduledoc """
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
    # 1) Find all users that are involved in this thread
    with post when not is_nil(post) <- Repo.preload(post, [thread: [posts: :user]]),
         thread when not is_nil(thread) <- post.thread,
         posts <- thread.posts,
         users <- Enum.map(posts, &(&1.user)) |> Enum.uniq do
           # 2) Send each of them an email about it
           for user <- users do
             Emails.thread_new_post_notification(user, post.thread, post)
             |> Mailer.deliver_now
           end
    else
      _ ->
        :ok
    end
    # 3) Get really angry users because this isn't remotely smart enough yet
    {:noreply, :nostate}
  end
end
