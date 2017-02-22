defmodule FirestormWeb.CategoryView do
  use FirestormWeb.Web, :view
  # LOL PLEASE DO NOT MAKE QUERIES IN THE VIEW
  alias FirestormData.{Category, Repo, Thread}
  import Ecto.Query, only: [from: 2]

  def children(category) do
    Category.children(category)
    |> Repo.all
    |> Enum.map(fn c ->
      Repo.preload(c, [threads: [:posts]])
    end)
  end
end
