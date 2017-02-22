defmodule FirestormData.Commands.GetHomeCategories do
  use FirestormData.Command

  defstruct [:user_id]

  def run(%__MODULE__{user_id: _user_id}) do
    # FIXME: Don't fetch them all all the time!
    {:ok, Repo.all(Category) |> Repo.preload([threads: [:posts], parent: []])}
  end
end
