defmodule FirestormData.Commands.UnfollowThread do
  @moduledoc """
  A command to unfollow a `Thread`. This marks the `Thread` as no longer followed by the `User`.
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
        user = Repo.get(User, user_id)

        if Followable.followed_by?(thread, user) do
          "threads_follows"
          |> where(assoc_id: ^thread.id)
          |> where(user_id: ^user.id)
          |> Repo.delete_all()
          |> handle_result
        else
          :ok
        end
      false ->
        :ok
    end
  end

  def handle_result(_) do
    # We always assume it works, so...there's that
    :ok
  end
end
