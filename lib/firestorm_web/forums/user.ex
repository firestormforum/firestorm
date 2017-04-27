defmodule FirestormWeb.Forums.User do
  use Ecto.Schema

  schema "forums_users" do
    field :email, :string
    field :name, :string
    field :username, :string

    timestamps()
  end
end
