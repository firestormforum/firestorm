defmodule FirestormWeb.CategoryView do
  use FirestormWeb.Web, :view
  # LOL PLEASE DO NOT MAKE QUERIES IN THE VIEW

  def children(category) do
    category
    |> Category.children
    |> Repo.all
    |> Enum.map(fn c ->
      Repo.preload(c, [threads: [:posts]])
    end)
  end
end
