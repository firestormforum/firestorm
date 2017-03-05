defmodule FirestormData.Viewable do
  @moduledoc """
  Some of the entities in our data model are `Viewable`. This module defines
  functions for such things.
  """

  alias FirestormData.Repo

  def view_count(viewable) do
    viewable
    |> Ecto.assoc(:views)
    |> Repo.aggregate(:count, :id)
  end
end
