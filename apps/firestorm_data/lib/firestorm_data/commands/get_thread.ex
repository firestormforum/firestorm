defmodule FirestormData.Commands.GetThread do
  @moduledoc """
  A `Command` to get a thread and preload the appropriate information
  """

  use FirestormData.Command
  import Ecto.Query

  defstruct [:finder, :category_id]

  def run(%__MODULE__{finder: finder, category_id: category_id}) when is_integer(finder) do
    do_run({:id, finder, category_id})
  end

  def run(%__MODULE__{finder: finder, category_id: category_id}) when is_binary(finder) do
    do_run({:slug, finder, category_id})
  end

  defp do_run({finder_key, finder, category_id}) do
    query =
      Thread
      |> where(^[{finder_key, finder}])
      |> where(category_id: ^category_id)
      |> preload(posts: [:user])
      |> preload(:category)

    case Repo.one(query) do
      nil -> {:error, :not_found}
      t -> {:ok, t}
    end
  end
end


