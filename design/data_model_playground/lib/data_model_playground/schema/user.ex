defmodule DataModelPlayground.User do
  use Ecto.Schema
  import Ecto.Changeset
  alias DataModelPlayground.{Repo}

  schema "users" do
    field :username, :string

    timestamps
  end

  @required_fields ~w(username)a
  @optional_fields ~w()a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end
