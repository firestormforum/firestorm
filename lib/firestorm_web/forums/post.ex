defmodule FirestormWeb.Forums.Post do
  @moduledoc """
  Schema for forum posts.
  """

  use Ecto.Schema
  import Ecto.Changeset, warn: false
  import Ecto.Query
  alias FirestormWeb.Forums.{User, Thread, View}

  schema "forums_posts" do
    field(:body, :string)
    belongs_to(:thread, Thread)
    belongs_to(:user, User)
    has_many(:views, {"forums_posts_views", View}, foreign_key: :assoc_id)

    many_to_many(
      :viewers,
      User,
      join_through: "forums_posts_views",
      join_keys: [assoc_id: :id, user_id: :id]
    )

    field(:oembeds, :any, virtual: true)

    timestamps()
  end

  def changeset(%__MODULE__{} = post, attrs \\ %{}) do
    post
    |> cast(attrs, [:body, :thread_id, :user_id])
    |> validate_required([:body, :thread_id, :user_id])
  end

  def ordered(query) do
    query
    |> order_by([p], desc: p.inserted_at)
  end
end
