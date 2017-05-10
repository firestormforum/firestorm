defmodule FirestormWeb.Forums.Thread do
  use Ecto.Schema

  alias FirestormWeb.Forums.{Category, Post}

  schema "forums_threads" do
    field :title, :string
    belongs_to :category, Category
    has_many :posts, Post

    timestamps()
  end
end
