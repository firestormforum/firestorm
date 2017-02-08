defmodule DataModelPlayground.Commands.Register do
  alias DataModelPlayground.{User, Repo}

  defstruct [:username, :password]

  def run(%__MODULE__{username: username}) do
    changeset =
      %User{}
      |> User.changeset(%{username: username})

    case Repo.insert(changeset) do
      {:ok, user} ->
        {:ok, user.id}

      {:error, changeset} ->
        {:error, changeset.errors}
    end
  end
end
