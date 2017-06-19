defmodule FirestormWeb.Forums.Thread do
  use Ecto.Schema

  alias FirestormWeb.Forums.{Category, Post, Watch, User}
  alias FirestormWeb.Forums.Slugs.ThreadTitleSlug

  schema "forums_threads" do
    field :title, :string
    field :slug, ThreadTitleSlug.Type
    belongs_to :category, Category
    has_many :posts, Post
    has_many :watches, {"forums_threads_watches", Watch}, foreign_key: :assoc_id
    many_to_many :watchers, User, join_through: "forums_threads_watches", join_keys: [assoc_id: :id, user_id: :id]

    timestamps()
  end
end
