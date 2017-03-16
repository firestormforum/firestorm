defmodule FirestormData.Commands.Register do
  @moduledoc false
  use FirestormData.Command

  defstruct [:username, :password, :email, :name]

  def run(%__MODULE__{username: username, email: email, name: name}) do
    changeset =
      %User{}
      |> User.changeset(%{username: username, email: email, name: name})

    case Repo.insert(changeset) do
      {:ok, user} ->
        {:ok, user.id}

      {:error, changeset} ->
        {:error, changeset.errors}
    end
  end
end
