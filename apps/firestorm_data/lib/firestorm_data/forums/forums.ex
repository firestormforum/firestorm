defmodule FirestormData.Forums do
  alias FirestormData.{Repo, Thread, User}
  import Ecto.Query

  def get_user!(id), do: Repo.get!(User, id)

  def get_followed_threads!(user) do
    Thread
    |> join(:left, [threads], follows in assoc(threads, :follows))
    |> where([_, follows], follows.user_id == ^user.id)
    |> preload([threads, _], [:category, :posts])
    |> select([threads, _], threads)
    |> Repo.all
  end
end
