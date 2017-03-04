defmodule FirestormData.Commands.ViewThread do
  @moduledoc """
  A command to view a thread. This records evidence that a given user viewed a given thread at a given time.
  """

  use FirestormData.Command

  embedded_schema do
    field :user_id, :integer
    field :thread_id, :integer
  end

  @required_fields ~w(user_id thread_id)a
  @optional_fields ~w()a

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
        %{user_id: user_id, thread_id: thread_id} =
          changeset
          |> apply_changes

        thread = Repo.get(Thread, thread_id)

        thread
        |> Ecto.build_assoc(:views, %{user_id: user_id})
        |> Repo.insert
        |> handle_result(changeset)

      false ->
        {:error, changeset}
    end
  end

  def handle_result({:ok, view}, _changeset) do
    {:ok, view.id}
  end
  def handle_result({:error, changes}, changeset) do
    # need to do better than this
    {:error, Changeset.add_error(changeset, :user_id, "There was an error", changes.errors)}
  end
end
