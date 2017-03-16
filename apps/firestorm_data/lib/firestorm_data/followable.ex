defmodule FirestormData.Followable do
  @moduledoc """
  Some of the entities in our data model are `Followable`. This module defines
  functions for such things.
  """

  alias FirestormData.{Repo, User}
  import Ecto.Query

  def followed_by?(followable, user = %User{}) do
    follow_count(followable, user) > 0
  end

  def follower_ids(followable) do
    followable
    |> Ecto.assoc(:follows)
    |> select([f], f.user_id)
    |> Repo.all
  end

  def follow_count(followable) do
    followable
    |> Ecto.assoc(:follows)
    |> Repo.aggregate(:count, :id)
  end
  defp follow_count(followable, user = %User{}) do
    followable
    |> Ecto.assoc(:follows)
    |> where([f], f.user_id == ^user.id)
    |> Repo.aggregate(:count, :id)
  end
end
