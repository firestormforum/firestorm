defmodule FirestormWeb.PageView do
  use FirestormWeb.Web, :view
  alias FirestormData.{Category, Repo, Thread}
  import Ecto.Query, only: [from: 2]

  def image_path(path) do
    if Mix.env == :prod do
      "/images/#{path}"
    else
      "http://localhost:8080/assets/images/#{path}"
    end
  end

  def children(category) do
    Category.children(category) |> Repo.all
  end

  def threads(category) do
    query =
      from t in Thread,
      where: t.category_id == ^category.id,
      preload: [:posts]

    Repo.all(query) |> IO.inspect
  end

  def user(thread) do
    {:ok, user} = Thread.user(thread)
    user
  end
end
