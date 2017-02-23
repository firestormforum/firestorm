defmodule FirestormData.Commands.GetHomeCategories do
  use FirestormData.Command

  defstruct [:user_id]

  def run(%__MODULE__{user_id: _user_id}) do
    categories =
      Category.roots
      |> preload([threads: [:posts], parent: []])
      |> Repo.all
      |> Enum.map(&add_children/1)

    # FIXME: Don't fetch them all all the time!
    {:ok, categories}
  end

  defp add_children(category) do
    children =
      category
      |> Category.children
      |> preload([threads: [:posts], parent: []])
      |> Repo.all

    category
    |> Map.put(:children, children)
  end
end
