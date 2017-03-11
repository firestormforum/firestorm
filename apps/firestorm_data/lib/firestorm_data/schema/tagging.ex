defmodule FirestormData.Tagging do
  @moduledoc """
  A `Tagging` associates a thing in our system with a `Tag`
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "abstract table: taggings" do
    # This will be used by associations on each "concrete" table
    field :assoc_id, :integer
    field :tag_id, :integer

    timestamps()
  end

  @required_fields ~w(assoc_id tag_id)a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
  end
end
