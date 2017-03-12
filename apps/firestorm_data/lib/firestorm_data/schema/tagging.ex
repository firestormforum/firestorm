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

  def thread_changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:tag_id, name: :threads_taggings_assoc_id_tag_id_index)
  end

  # Since we have to have a different name for each unique constraint (because
  # they're complex - i.e. on 2 fields - we have to have a different changeset
  # for each type of thing we can tag.
  #
  # I don't love it, but that's where we are atm.
  def category_changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
    |> unique_constraint(:tag_id, name: :categories_taggings_assoc_id_tag_id_index)
  end
end
