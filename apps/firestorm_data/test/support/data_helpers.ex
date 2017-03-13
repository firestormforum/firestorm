defmodule FirestormData.DataHelpers do
  @moduledoc false
  alias FirestormData.{User, Repo}

  def create_user(_) do
    changeset =
      %User{}
      |> User.changeset(%{username: "sonny", email: "sonny@example.org"})

    {:ok, user} = Repo.insert(changeset)

    {:ok, user_id: user.id}
  end
end
