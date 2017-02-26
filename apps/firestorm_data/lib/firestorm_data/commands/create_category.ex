defmodule FirestormData.Commands.CreateCategory do
  @moduledoc """
  A `Command` for creating a `Category`
  """

  use FirestormData.Command

  embedded_schema do
    field :title, :string
    field :parent_id, :integer
  end

  @required_fields ~w(title)a
  @optional_fields ~w(parent_id)a

  def changeset(record, params \\ %{}) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  # Imagine we've extracted this to something like `Firestorm.run`
  # and it can handle all of our commands.
  #
  # For now we'll just put them in each command til I figure it out :)
  def run(changeset) do
    case changeset.valid? do
      true ->
        %{title: title, parent_id: parent_id} =
          changeset
          |> apply_changes

        %Category{}
        |> Category.changeset(%{title: title, parent_id: parent_id})
        |> Repo.insert
        |> handle_result(changeset)

      false ->
        {:error, changeset}
    end
  end

  defp handle_result({:ok, c}, _changeset), do: {:ok, c.id}
  defp handle_result({:error, changes}, changeset) do
    # need to do better than this
    {:error, Changeset.add_error(changeset, :title, "There was an error", changes.errors)}
  end
end
