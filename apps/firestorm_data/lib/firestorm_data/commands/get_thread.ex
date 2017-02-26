defmodule FirestormData.Commands.GetThread do
  @moduledoc """
  A `Command` to get a thread and preload the appropriate information
  """

  use FirestormData.Command
  import Ecto.Query

  defstruct [:finder, :category]

  def run(%__MODULE__{finder: finder, category: category}) when is_integer(finder) do
    do_run({:id, finder, category})
  end

  def run(%__MODULE__{finder: finder, category: category}) when is_binary(finder) do
    do_run({:slug, finder, category})
  end

  defp do_run({finder_key, finder, category}) do
    query =
      Thread
      |> where(^[{finder_key, finder}, category_id: category.id])
      |> preload([posts: [:user], category: []])

    case Repo.one(query) do
      nil -> {:error, :not_found}
      t -> {:ok, t}
    end
  end
end


