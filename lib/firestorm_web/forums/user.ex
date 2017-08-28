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
    field :api_token, :string

    has_many :posts, Post

    timestamps()
  end

  def avatar_url(%__MODULE__{email: email, username: username}, size \\ 256) do
    if email do
      gravatar_url(email, username, size)
    else
      adorable_url(username, size)
    end
  end

  defp gravatar_url(email, username, size) do
    email
    |> Exgravatar.gravatar_url(d: adorable_url(username, size), s: size)
  end

  defp adorable_url(username, size) do
    "https://api.adorable.io/avatars/#{size}/#{username}@adorable.png"
  end
end
