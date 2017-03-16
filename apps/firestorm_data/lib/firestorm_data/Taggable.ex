defmodule FirestormData.Taggable do
  @moduledoc """
  Some of the entities in our data model are `Taggable`. This module defines
  functions for such things.
  """

  alias FirestormData.Repo
  import Ecto.Query

  def tagged_with?(taggable, tag) do
    count =
      taggable
      |> Ecto.assoc(:taggings)
      |> where([t], t.tag_id == ^tag.id)
      |> Repo.aggregate(:count, :id)

    count > 0
  end
end
