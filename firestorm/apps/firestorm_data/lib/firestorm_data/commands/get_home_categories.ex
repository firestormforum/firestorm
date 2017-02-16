defmodule FirestormData.Commands.GetHomeCategories do
  alias FirestormData.{Category, Repo}
  import Ecto.Query, only: [from: 2]

  defstruct [:user_id]

  def run(%__MODULE__{user_id: _user_id}) do
    # FIXME: Don't fetch them all all the time!
    {:ok, Repo.all(Category)}
  end
end
