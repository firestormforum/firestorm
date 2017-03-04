defmodule FirestormData.Viewable do
  alias FirestormData.Repo

  def view_count(viewable) do
    viewable
    |> Ecto.assoc(:views)
    |> Repo.aggregate(:count, :id)
  end
end
