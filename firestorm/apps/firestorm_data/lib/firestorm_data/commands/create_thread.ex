defmodule FirestormData.Commands.CreateThread do
  use FirestormData.Command

  defstruct [:title]

  def run(%__MODULE__{title: title}) do
    changeset =
      %Category{}
      |> Category.changeset(%{title: title})

    case Repo.insert(changeset) do
      {:ok, category} ->
        {:ok, category.id}

      {:error, changeset} ->
        {:error, changeset.errors}
    end
  end
end
