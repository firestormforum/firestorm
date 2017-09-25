defmodule FirestormWeb.Forums.View do
  @moduledoc """
  A `View` is a polymorphic representation that a user viewed a thing in our
  system at a specified time.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "abstract table: views" do
    # This will be used by associations on each "concrete" table
    field :assoc_id, :integer
    field :user_id, :integer

    timestamps()
  end

  def changeset(%__MODULE__{} = view, attrs) do
    view
    |> cast(attrs, [:assoc_id, :user_id])
    |> validate_required([:assoc_id, :user_id])
  end
end
