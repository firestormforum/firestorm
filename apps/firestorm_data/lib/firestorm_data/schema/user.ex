defmodule FirestormData.User do
  use Ecto.Schema

  schema "users" do
    field :username, :string
    field :name, :string
    field :email, :string

    timestamps()
  end
end
