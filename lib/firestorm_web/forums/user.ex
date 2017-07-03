defmodule FirestormWeb.Forums.User do
  @moduledoc """
  Schema for forum users.
  """

  use Ecto.Schema

  alias FirestormWeb.Forums.Post

  schema "forums_users" do
    field :email, :string
    field :name, :string
    field :username, :string
    field :password_hash, :string
    field :password, :string, virtual: true

    has_many :posts, Post

    timestamps()
  end
end
