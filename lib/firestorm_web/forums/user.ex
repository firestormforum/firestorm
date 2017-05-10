defmodule FirestormWeb.Forums.User do
  use Ecto.Schema

  alias FirestormWeb.Forums.Post

  schema "forums_users" do
    field :email, :string
    field :name, :string
    field :username, :string
    has_many :posts, Post

    timestamps()
  end
end
