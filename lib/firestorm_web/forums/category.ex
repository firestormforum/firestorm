defmodule FirestormWeb.Forums.Category do
  use Ecto.Schema

  schema "forums_categories" do
    field :title, :string

    timestamps()
  end
end
