defmodule FirestormData.Commands.GetCategory do
  use FirestormData.Command
  import Ecto.Query

  defstruct [:finder]

  def run(%__MODULE__{finder: finder}) do
    finder_key =
      case Integer.parse(finder) do
        :error ->
          :slug
        _ ->
          :id
      end

    query =
      Category
      |> where(^[{finder_key, finder}])
      |> preload([:threads, :parent])

    case Repo.one(query) do
      nil -> {:error, :not_found}
      c -> {:ok, c}
    end
  end
end
