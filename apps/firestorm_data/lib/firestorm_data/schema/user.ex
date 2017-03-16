defmodule FirestormData.User do
  @moduledoc """
  A user in our system
  """

  use Ecto.Schema
  import Ecto.Changeset
  import FirestormData.{View}

  schema "users" do
    field :username, :string
    field :name, :string
    field :email, :string

    timestamps()
  end

  @required_fields ~w(username email)a
  @optional_fields ~w(name)a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:username)
  end
end
