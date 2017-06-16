defmodule FirestormWeb.Forums.Category do
  use Ecto.Schema

  alias FirestormWeb.Forums.Thread
  alias FirestormWeb.Forums.Slugs.CategoryTitleSlug

  schema "forums_categories" do
    field :title, :string
    field :slug, CategoryTitleSlug.Type
    has_many :threads, Thread

    timestamps()
  end
end
