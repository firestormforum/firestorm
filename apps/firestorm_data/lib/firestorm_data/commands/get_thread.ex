defmodule FirestormData.Commands.GetThread do
  @moduledoc """
  A `Command` to get a thread and preload the appropriate information
  """

  use FirestormData.Command
  import Ecto.Query
  alias FirestormData.Commands.GetCategory
  alias FirestormData.Category

  defstruct [:finder, :category_finder]

  def run(%__MODULE__{finder: finder, category_finder: category_finder}) do
    thread_finder_tuple = get_finder_tuple(finder)
    category_finder_tuple = get_category_finder_tuple(category_finder)

    do_run(thread_finder_tuple, category_finder_tuple)
  end

  defp get_finder_tuple(finder) when is_integer(finder) do
    {:id, finder}
  end
  defp get_finder_tuple(finder) when is_binary(finder) do
    {:slug, finder}
  end

  defp get_category_finder_tuple(finder) when is_integer(finder) do
    {:category_id, finder}
  end
  defp get_category_finder_tuple(finder) when is_binary(finder) do
    {:ok, category} =
      %GetCategory{finder: finder}
      |> GetCategory.run

    {:category_id, category.id}
  end

  defp do_run({finder_key, finder}, {category_finder_key, category_finder}) do
    query =
      Thread
      |> where(^[{finder_key, finder}])
      |> where(^[{category_finder_key, category_finder}])
      |> preload(posts: [:user, :reactions])
      |> preload(:category)
      |> preload(:tags)

    case Repo.one(query) do
      nil ->
        {:error, :not_found}
      t ->
        # NOTE: The category is never preloaded for some reason unless I do it via
        # Repo rather than Query
        thread =
          t |> Repo.preload(:category)

          ancestors =
            thread.category
            |> Category.ancestors
            |> Repo.all

          thread =
            %Thread{thread |
              category: %Category{thread.category |
                ancestors: ancestors
              }
            }

        {:ok, thread}
    end
  end
end
