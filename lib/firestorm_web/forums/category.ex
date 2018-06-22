defmodule FirestormWeb.Forums.Category do
  @moduledoc """
  Schema for forum categories.
  """

  use Ecto.Schema
  import Ecto.Changeset, warn: false

  alias FirestormWeb.Forums.Thread
  alias FirestormWeb.Forums.Slugs.CategoryTitleSlug

  schema "forums_categories" do
    field(:title, :string)
    field(:slug, CategoryTitleSlug.Type)
    has_many(:threads, Thread)

    timestamps()
  end

  def changeset(%__MODULE__{} = category, attrs \\ %{}) do
    category
    |> cast(attrs, [:title])
    |> validate_required([:title])
    |> CategoryTitleSlug.maybe_generate_slug()
    |> CategoryTitleSlug.unique_constraint()
  end
end
