defmodule FirestormData.Commands.LoginOrRegisterFromGitHub do
  use FirestormData.Command

  def run(%{username: username}) do
    case find_user_by_username(username) do
      nil -> create_user(username)
      u -> {:ok, u}
    end
  end

  defp find_user_by_username(username) do
    query =
      from u in User,
        where: u.username == ^username

    Repo.one query
  end

  defp create_user(username) do
    result =
      %User{}
      |> User.changeset(%{username: username})
      |> Repo.insert

    case result do
      {:ok, u} ->
        {:ok, u}
      {:error, changeset} ->
        {:error, changeset.errors}
    end
  end
end
