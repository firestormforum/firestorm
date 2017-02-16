defmodule FirestormData.Category do
  defmodule TitleSlug do
    use EctoAutoslugField.Slug, from: :title, to: :slug
  end

  use Ecto.Schema
  import Ecto.Changeset
  alias FirestormData.Repo
  use Arbor.Tree

  schema "categories" do
    field :title, :string
    field :slug, TitleSlug.Type
    belongs_to :parent, __MODULE__

    timestamps()
  end

  @required_fields ~w(title)a
  @optional_fields ~w(parent_id slug)a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> TitleSlug.maybe_generate_slug
    |> TitleSlug.unique_constraint
  end

  def categories do
    Repo.all(__MODULE__)
  end

  def add(title) do
    changeset(%__MODULE__{}, %{title: title})
    |> Repo.insert
  end
end
