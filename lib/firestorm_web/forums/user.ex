defmodule FirestormWeb.Forums.User do
  @moduledoc """
  Schema for forum users.
  """

  use Ecto.Schema

  import Ecto.Changeset, warn: false
  alias FirestormWeb.Forums.{Post, RoleMembership, Role}

  schema "forums_users" do
    field :email, :string
    field :name, :string
    field :username, :string
    field :password_hash, :string
    field :password, :string, virtual: true
    field :api_token, :string

    has_many :posts, Post
    has_many :role_memberships, RoleMembership
    many_to_many :roles, Role, join_through: RoleMembership, on_replace: :delete

    timestamps()
  end

  def changeset(%__MODULE__{} = user, attrs \\ %{}) do
    user
    |> cast(attrs, [:username, :email, :name, :api_token])
    |> validate_required([:username, :name, :api_token])
    |> unique_constraint(:username)
  end

  def admin_changeset(%__MODULE__{} = user, attrs \\ %{}) do
    user
    |> changeset(attrs)
    |> add_roles(attrs)
  end

  def add_roles(changeset, params) do
    import Ecto.Query

    if Enum.count(Map.get(params, :roles, [])) > 0 do
      ids = params[:roles]
      # FIXME: I don't love going out to the repo here
      roles = FirestormWeb.Repo.all(from r in Role, where: r.id in ^ids)
      put_assoc(changeset, :roles, roles)
    else
      changeset
    end
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
