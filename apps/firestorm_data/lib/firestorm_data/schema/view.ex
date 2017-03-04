defmodule FirestormData.View do
  @moduledoc """
  A `View` is a polymorphic representation that a user has viewed a thing in our system.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "abstract table: views" do
    # This will be used by associations on each "concrete" table
    field :assoc_id, :integer
    field :user_id, :integer

    timestamps()
  end

  @required_fields ~w(assoc_id user_id)a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
  end
end
