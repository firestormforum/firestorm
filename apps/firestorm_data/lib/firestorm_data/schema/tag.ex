defmodule FirestormData.Tag do
  @moduledoc """
  A `Tag` is a rough way to organize items by giving them essentially free-form labels.
  """

  defmodule TitleSlug do
    @moduledoc """
    A configuration for turning tag titles into slugs.
    """

    use EctoAutoslugField.Slug, from: :title, to: :slug
  end

  use Ecto.Schema
  import Ecto.Changeset

  schema "tags" do
    field :title, :string
    field :slug, TitleSlug.Type

    timestamps()
  end

  @required_fields ~w(title)a
  @optional_fields ~w(slug)a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> TitleSlug.maybe_generate_slug
    |> TitleSlug.unique_constraint
  end

  def color(category) do
    category
    |> hash_number
  end

  def hash(category) do
    :crypto.hash(:sha, category.slug)
  end

  def hashlist(category) do
    for <<num <- hash(category)>>, do: num
  end

  def hash_number(category) do
    category
    |> hashlist
    |> Enum.sum
    |> rem(360)
  end
end
