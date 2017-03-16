defmodule FirestormData.Tag do
  @moduledoc """
  A `Tag` is a rough way to organize items by giving them essentially free-form labels.
  """

  defmodule TitleSlug do
    @moduledoc """
    A configuration for turning tag titles into slugs.
    """

    use EctoAutoslugField.Slug, from: :title, to: :slug
    alias FirestormData.{Repo, Tag}
    import Ecto.{Query}

    def build_slug(sources) do
      base_slug = super(sources)
      get_unused_slug(base_slug, 0)
    end

    def get_unused_slug(base_slug, number) do
      slug = get_slug(base_slug, number)
      if slug_used?(slug) do
        get_unused_slug(base_slug, number + 1)
      else
        slug
      end
    end

    def slug_used?(slug) do
      Tag
      |> where(slug: ^slug)
      |> Repo.one
    end

    def get_slug(base_slug, 0), do: base_slug
    def get_slug(base_slug, number) do
      "#{base_slug}-#{number}"
    end
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
