defmodule DataModelPlayground.Category do
  use Ecto.Schema
  import Ecto.Changeset
  alias DataModelPlayground.Repo

  schema "categories" do
    field :title, :string
    timestamps
  end

  @required_fields ~w(title)
  @optional_fields ~w()

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields, @optional_fields)
  end

  def categories do
    Repo.all(__MODULE__)
  end

  def add(title) do
    changeset(%__MODULE__{}, %{title: title})
      |> Repo.insert
  end
end
