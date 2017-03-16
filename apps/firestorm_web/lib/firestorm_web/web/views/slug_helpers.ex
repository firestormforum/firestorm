defmodule FirestormWeb.Web.SlugHelpers do
  @moduledoc """
  View helpers for working with slugs in URLs and routes.
  """
  def slugged_finder(nil, id), do: id
  def slugged_finder("", id), do: id
  def slugged_finder(slug, _), do: slug

  @doc """
  Choose the slug or id for finding a category in a route URL.
  """
  def category_finder(category) do
    slugged_finder(category.slug, category.id)
  end

  @doc """
  Choose the slug or id for finding a thread in a route URL. NOTE: This is a placeholder, presently stubbed out to always return the id
  """
  def thread_finder(thread) do
    thread.id
  end
end
