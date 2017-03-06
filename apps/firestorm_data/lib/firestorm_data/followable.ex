defmodule FirestormData.Followable do
  @moduledoc """
  Some of the entities in our data model are `Followable`. This module defines
  functions for such things.
  """

  alias FirestormData.{Repo, User}
  import Ecto.Query

  def followed_by?(followable, user=%User{}) do
    follow_count =
      followable
      |> Ecto.assoc(:follows)
      |> where([f], f.user_id == ^user.id)
      |> Repo.aggregate(:count, :id)

    follow_count > 0
  end
end
