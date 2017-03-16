defmodule FirestormData.Commands.GetHomeCategories do
  @moduledoc """
  A `Command` to get the categories to be shown on the home screen. It accepts a
  user_id, as we need to know the user that is requesting the list in order to
  specify how many new posts have occurred since that user last saw the thread.
  """

  use FirestormData.Command

  defstruct [:user_id]

  def run(%__MODULE__{user_id: _user_id}) do
    categories =
      Category.roots
      |> preload([threads: [posts: [:user], category: []], parent: []])
      |> Repo.all
      |> Enum.map(&add_children/1)

    {:ok, categories}
  end

  defp add_children(category) do
    children =
      category
      |> Category.children
      |> preload([threads: [posts: [:user], category: []], parent: []])
      |> Repo.all

    category
    |> Map.put(:children, children)
  end
end
