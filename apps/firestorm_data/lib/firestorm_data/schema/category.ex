defmodule FirestormData.Category do
  @moduledoc """
  A `Category` is a grouping of related `Thread`s. Categories are modeled as a
  Forest of Trees.
  """

  defmodule TitleSlug do
    @moduledoc """
    A configuration for turning category titles into slugs.
    """

    use EctoAutoslugField.Slug, from: :title, to: :slug
    alias FirestormData.{Repo, Category}
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
      Category
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
  alias FirestormData.{Repo, Thread, View, Follow, Tagging, Tag}
  use Arbor.Tree

  schema "categories" do
    field :title, :string
    field :slug, TitleSlug.Type
    field :children, :any, virtual: true
    belongs_to :parent, __MODULE__
    has_many :threads, Thread
    has_many :views, {"categories_views", View}, foreign_key: :assoc_id
    has_many :follows, {"categories_follows", Follow}, foreign_key: :assoc_id
    has_many :taggings, {"categories_taggings", Tagging}, foreign_key: :assoc_id
    many_to_many :tags, Tag, join_through: "categories_taggings", join_keys: [assoc_id: :id, tag_id: :id]

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
