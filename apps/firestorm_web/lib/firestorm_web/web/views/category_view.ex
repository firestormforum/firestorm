defmodule FirestormWeb.Web.CategoryView do
  use FirestormWeb.Web, :view
  def children(category) do
    category
    |> Category.children
    |> Repo.all
    |> Enum.map(fn c ->
      Repo.preload(c, [threads: [:posts]])
    end)
  end
end
