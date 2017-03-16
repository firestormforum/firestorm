defmodule FirestormData.Commands.CreatePost do
  @moduledoc """
  A command to create a `Post` in a `Thread`.
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

  def handle_result({:ok, post}, _changeset) do
    # Get the post and its user and thread and notify
    post =
      Post
      |> where(id: ^post.id)
      |> preload(thread: [category: []])
      |> preload(:user)
      |> Repo.one

    Events.notify({:new_post, post})
    {:ok, post.id}
  end
  def handle_result({:error, changes}, changeset) do
    # FIXME: We really don't know what to do with these changesets yet sigh
    {:error, Changeset.add_error(changeset, :body, changes[:body].errors)}
  end
end
