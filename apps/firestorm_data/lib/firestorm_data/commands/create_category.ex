defmodule FirestormData.Commands.CreateCategory do
  @moduledoc """
  A `Command` for creating a `Category`
  """

  use FirestormData.Command

  defstruct [:title, :parent_id]

  def run(%__MODULE__{title: title, parent_id: parent_id}) do
    changeset =
      %Category{}
      |> Category.changeset(%{title: title, parent_id: parent_id})

    case Repo.insert(changeset) do
      {:ok, category} ->
        {:ok, category.id}

      {:error, changeset} ->
        {:error, changeset.errors}
    end
  end
end
