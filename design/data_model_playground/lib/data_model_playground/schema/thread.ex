defmodule DataModelPlayground.Thread do
  use Ecto.Schema
  import Ecto.Changeset
  alias DataModelPlayground.{Repo, Category}

  schema "threads" do
    belongs_to :category, Category

    timestamps
  end

  @required_fields ~w(category_id)a
  @optional_fields ~w()a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end
