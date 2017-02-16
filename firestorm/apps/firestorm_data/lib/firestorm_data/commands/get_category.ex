defmodule FirestormData.Commands.GetCategory do
  alias FirestormData.{Category, Repo}
  import Ecto.Query, only: [from: 2]

  defstruct [:finder]

  def run(%__MODULE__{finder: finder}) when is_integer(finder) do
    case Repo.get(Category, finder) do
      nil -> {:error, :not_found}
      c -> {:ok, c}
    end
  end
  def run(%__MODULE__{finder: finder}) when is_binary(finder) do
    query =
      from c in Category,
        where: c.slug == ^finder

    case Repo.one(query) do
      nil -> {:error, :not_found}
      c -> {:ok, c}
    end
  end
end
