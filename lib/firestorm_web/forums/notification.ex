defmodule FirestormWeb.Forums.Notification do
  @moduledoc """
  Schema for notifications.
  """

  use Ecto.Schema
  require FirestormWeb.Web.Router
  import FirestormWeb.Web.Router.Helpers, only: [category_thread_url: 4]
  alias FirestormWeb.Web.Router.Helpers
  alias FirestormWeb.Forums.{User, Thread, Post}
  alias FirestormWeb.Web.Endpoint
  import Ecto.Changeset

  schema "forums_notifications" do
    field :body, :string
    field :subject, :string
    field :url, :string
    belongs_to :user, User

    timestamps()
  end

  def thread_new_post_notification(%Thread{} = thread, %Post{} = post) do
    %{
      subject: "There was a new post in thread: #{thread.title}",
      body: post.body,
      url: post_url(thread, post)
    }
  end

  defp post_url(thread, post) do
    "#{thread_url(thread)}#post-#{post.id}"
  end

  defp thread_url(thread) do
    category_thread_url(Endpoint, :show, thread.category_id, thread.id)
  end

  def changeset(%__MODULE__{} = notification, attrs) do
    notification
    |> cast(attrs, [:user_id, :body, :subject, :url])
    |> validate_required([:user_id, :body, :subject, :url])
  end
end
