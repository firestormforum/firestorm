defmodule FirestormData.Commands.CreatePost do
  @moduledoc """
  A command to create a post in a thread.
  """

  use FirestormData.Command

  embedded_schema do
    field :thread_id, :integer
    field :user_id, :integer
    field :body, :string
  end

  @required_fields ~w(user_id thread_id body)a
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
        %{user_id: user_id, thread_id: thread_id, body: body} =
          changeset
          |> apply_changes

        %Post{}
        |> Post.changeset(%{body: body, user_id: user_id, thread_id: thread_id})
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
