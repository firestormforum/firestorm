defmodule FirestormData.Commands.CreateReaction do
  @moduledoc """
  A command to react to a `Post`. This records a given `User` reacting to a `Post` with some emoji.
  """

  use FirestormData.Command
  alias FirestormData.Reaction

  embedded_schema do
    field :user_id, :integer
    field :post_id, :integer
    field :emoji, :string
  end

  @required_fields ~w(user_id post_id emoji)a
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
        %{user_id: user_id, post_id: post_id, emoji: emoji} =
          changeset
          |> apply_changes

        post = Repo.get(Post, post_id)

        post
        |> Ecto.build_assoc(:reactions, %{user_id: user_id, emoji: emoji})
        |> Reaction.changeset(%{})
        |> Repo.insert
        |> handle_result(changeset)

      false ->
        {:error, changeset}
    end
  end

  def handle_result({:ok, reaction}, _changeset) do
    {:ok, reaction.id}
  end
  def handle_result({:error, changes}, changeset) do
    # need to do better than this
    {:error, Changeset.add_error(changeset, :user_id, "There was an error", changes.errors)}
  end
end
