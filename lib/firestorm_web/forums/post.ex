defmodule FirestormWeb.Forums.Post do
  @moduledoc """
  Schema for forum posts.
  """

  use Ecto.Schema
  alias FirestormWeb.Forums.{User, Thread, View}

  schema "forums_posts" do
    field :body, :string
    belongs_to :thread, Thread
    belongs_to :user, User
    has_many :views, {"forums_posts_views", View}, foreign_key: :assoc_id
    many_to_many :viewers, User, join_through: "forums_posts_views", join_keys: [assoc_id: :id, user_id: :id]

    timestamps()
  end
end
