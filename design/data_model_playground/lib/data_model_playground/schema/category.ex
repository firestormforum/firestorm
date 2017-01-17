defmodule DataModelPlayground.Category do
  use Ecto.Schema
  import Ecto.Changeset
  alias DataModelPlayground.Repo
  use Arbor.Tree

  schema "categories" do
    field :title, :string
    belongs_to :parent, __MODULE__

    timestamps
  end

  @required_fields ~w(title)a
  @optional_fields ~w(parent_id)a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def categories do
    Repo.all(__MODULE__)
  end

  def add(title) do
    changeset(%__MODULE__{}, %{title: title})
      |> Repo.insert
  end
end
