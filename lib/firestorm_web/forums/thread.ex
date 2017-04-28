defmodule FirestormWeb.Forums.Thread do
  use Ecto.Schema

  schema "forums_threads" do
    field :title, :string
    field :category_id, :id

    timestamps()
  end
end
