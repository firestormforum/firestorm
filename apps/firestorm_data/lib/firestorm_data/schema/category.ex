defmodule FirestormData.Category do
  use Ecto.Schema
  import Ecto.Changeset

  schema "categories" do
    field :title, :string

    timestamps()
  end

  def changeset(category, params \\ %{}) do
    category
    |> cast(params, [:title])
  end
end
