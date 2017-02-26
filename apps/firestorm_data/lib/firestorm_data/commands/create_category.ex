defmodule FirestormData.Commands.CreateCategory do
  @moduledoc """
  A `Command` for creating a `Category`
  """

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
