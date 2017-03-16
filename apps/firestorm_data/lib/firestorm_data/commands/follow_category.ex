defmodule FirestormData.Commands.FollowCategory do
  @moduledoc """
  A command to follow a `Category`. This marks the `Category` as followed by the `User`.
  """

  use FirestormData.Command
  alias FirestormData.Follow

  embedded_schema do
    field :user_id, :integer
    field :category_id, :integer
  end

  @required_fields ~w(user_id category_id)a
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
        %{user_id: user_id, category_id: category_id} =
          changeset
          |> apply_changes

        category = Repo.get(Category, category_id)
        user = Repo.get(User, user_id)

        if Followable.followed_by?(category, user) do
          {:error, Changeset.add_error(changeset, :user_id, "User already follows this category")}
        else
          category
          |> Ecto.build_assoc(:follows, %{user_id: user_id})
          |> Follow.changeset(%{})
          |> Repo.insert
          |> handle_result(changeset)
        end

      false ->
        {:error, changeset}
    end
  end

  def handle_result({:ok, follow}, _changeset) do
    {:ok, follow.id}
  end
  def handle_result({:error, changes}, changeset) do
    # need to do better than this
    {:error, Changeset.add_error(changeset, :user_id, "There was an error", changes.errors)}
  end
end
