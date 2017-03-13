defmodule FirestormData.Commands.CreatePost do
  @moduledoc """
  A command to create a `Post`.
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
        |> Post.changeset(%{user_id: user_id, thread_id: thread_id, body: body})
        |> Repo.insert
        |> handle_result(changeset)

      false ->
        {:error, changeset}
    end
  end

  def handle_result({:ok, post}, _changeset) do
    {:ok, post.id}
  end
  def handle_result({:error, changes}, changeset) do
    # FIXME: We really don't know what to do with these changesets yet sigh
    {:error, Changeset.add_error(changeset, :body, changes[:body].errors)}
  end
end
