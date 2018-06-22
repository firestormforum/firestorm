defmodule FirestormWeb.Forums.Role do
  @moduledoc """
  Schema for forum roles.
  """

  use Ecto.Schema
  import Ecto.Changeset, warn: false
  alias FirestormWeb.Forums.{User, RoleMembership}

  schema "forums_roles" do
    field(:name, :string)
    has_many(:role_memberships, RoleMembership)
    many_to_many(:users, User, join_through: RoleMembership)

    timestamps()
  end

  def changeset(%__MODULE__{} = role, attrs \\ %{}) do
    role
    |> cast(attrs, [:name])
    |> validate_required([:name])
    |> unique_constraint(:name)
  end
end
