defmodule FirestormWeb.Forums.Thread do
  @moduledoc """
  Schema for forum threads.
  """

  use Ecto.Schema
  import Ecto.Changeset, warn: false

  alias FirestormWeb.Forums.{Category, Post, Watch, User}
  alias FirestormWeb.Forums.Slugs.ThreadTitleSlug

  schema "forums_threads" do
    field :title, :string
    field :slug, ThreadTitleSlug.Type
    field :first_post, {:map, %Post{}}, virtual: true
    field :posts_count, :integer, virtual: true
    field :completely_read?, :boolean, virtual: true
    belongs_to :category, Category
    has_many :posts, Post
    has_many :watches, {"forums_threads_watches", Watch}, foreign_key: :assoc_id
    many_to_many :watchers, User, join_through: "forums_threads_watches", join_keys: [assoc_id: :id, user_id: :id]

    timestamps()
  end

  def changeset(%__MODULE__{} = thread, attrs \\ %{}) do
    thread
    |> cast(attrs, [:title, :category_id])
    |> validate_required([:title, :category_id])
    |> ThreadTitleSlug.maybe_generate_slug
    |> ThreadTitleSlug.unique_constraint
  end

  def new_changeset(%{thread: thread_attrs, post: post_attrs}) do
    post_changeset =
      %Post{}
      |> cast(post_attrs, [:body, :user_id])
      |> validate_required([:body, :user_id])

    %__MODULE__{}
    |> changeset(thread_attrs)
    |> put_assoc(:posts, [post_changeset])
  end
end
