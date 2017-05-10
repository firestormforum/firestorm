defmodule FirestormWeb.Forums.Category do
  use Ecto.Schema

  alias FirestormWeb.Forums.Thread

  schema "forums_categories" do
    field :title, :string
    has_many :threads, Thread

    timestamps()
  end
end
