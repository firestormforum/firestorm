defmodule FirestormData.Commands.GetCategory do
  @moduledoc """
  A `Command` to get a category by either id or slug.
  """

  use FirestormData.Command
  import Ecto.Query

  defstruct [:finder]

  def run(%__MODULE__{finder: finder}) when is_integer(finder) do
    do_run({:id, finder})
  end

  def run(%__MODULE__{finder: finder}) when is_binary(finder) do
    do_run({:slug, finder})
  end

  defp do_run({finder_key, finder}) do
    query =
      Category
      |> where(^[{finder_key, finder}])
      |> preload(threads: [posts: [user: []], category: []])
      |> preload(:parent)
      |> preload(:tags)

    case Repo.one(query) do
      nil -> {:error, :not_found}
      c ->
        c = Map.put(c, :children, get_children(c))
        {:ok, c}
    end
  end

  defp get_children(category) do
    category
    |> Category.children
    |> Repo.all
    |> Enum.map(fn c ->
      Repo.preload(c, [threads: [:posts]])
    end)
  end
end
