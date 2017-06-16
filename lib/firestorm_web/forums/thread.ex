defmodule FirestormWeb.Forums.Thread do
  use Ecto.Schema

  alias FirestormWeb.Forums.{Category, Post}
  alias FirestormWeb.Forums.Slugs.ThreadTitleSlug

  schema "forums_threads" do
    field :title, :string
    field :slug, ThreadTitleSlug.Type
    belongs_to :category, Category
    has_many :posts, Post

    timestamps()
  end
end
