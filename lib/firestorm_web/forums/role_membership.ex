defmodule FirestormWeb.Forums.RoleMembership do
  @moduledoc """
  Schema for forum role memberships.
  """

  use Ecto.Schema
  import Ecto.Changeset, warn: false
  alias FirestormWeb.Forums.{Role, User}

  schema "forums_role_memberships" do
    belongs_to :role, Role
    belongs_to :user, User

    timestamps()
  end

  def changeset(%__MODULE__{} = role_membership, attrs \\ %{}) do
    role_membership
    |> cast(attrs, [:user_id, :role_id])
    |> validate_required([:user_id, :role_id])
  end
end
