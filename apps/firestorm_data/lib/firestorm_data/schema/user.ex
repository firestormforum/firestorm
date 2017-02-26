defmodule FirestormData.User do
  @moduledoc """
  A user in our system
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :username, :string

    timestamps()
  end

  @required_fields ~w(username)a
  @optional_fields ~w()a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:username)
  end
end
