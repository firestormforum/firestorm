defmodule FirestormWeb.Web.SlugHelpers do
  @moduledoc """
  View helpers for working with slugs.
  """
  def slugged_finder(nil, id), do: id
  def slugged_finder("", id), do: id
  def slugged_finder(slug, _), do: slug

  def category_finder(category) do
    slugged_finder(category.slug, category.id)
  end
end
