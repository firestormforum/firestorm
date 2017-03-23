defmodule FirestormData.Commands.CreateThread do
  @moduledoc """
  A command to create a thread. This will create both the thread and its first
  post, as we should never have a thread without a first post.
  """

  use FirestormData.Command

  embedded_schema do
    field :category_id, :integer
    field :user_id, :integer
    field :title, :string
    field :body, :string
  end

  @required_fields ~w(user_id category_id title body)a
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
        %{user_id: user_id, title: title, category_id: category_id, body: body} =
          changeset
          |> apply_changes

        Multi.new
        |> Multi.insert(:thread, thread_changeset(user_id, category_id, title))
        |> Multi.run(:post, &insert_post(&1, user_id, body))
        |> Repo.transaction
        |> handle_result(changeset)

      false ->
        {:error, changeset}
    end
  end

  def handle_result({:ok, %{thread: thread, post: _post}}, _changeset) do
    first_post = Thread.first_post(thread)

    thread =
      thread
      |> Repo.preload([
        category: [threads: [posts: [:user]]],
        posts: [:user]
      ])

    {posts, users} =
      case first_post do
        nil -> {[], []}
        post -> {[post], [post.user]}
      end
    payload =
      %{
        categories: [thread.category],
        threads: [thread],
        users: users,
        posts: posts
      }

    # Tell connected clients about the new thread - also update their category
    broadcast_endpoint().broadcast! "categories:#{thread.category_id}", "update", payload

    {:ok, thread.id}
  end
  def handle_result({:error, errored_key, changes}, changeset) do
    {:error, Changeset.add_error(changeset, errored_key, changes[errored_key].errors)}
  end

  defp insert_post(%{thread: %{id: thread_id}}, user_id, body) do
      %Post{}
      |> Post.changeset(%{user_id: user_id, thread_id: thread_id, body: body})
      |> Repo.insert
  end

  defp thread_changeset(user_id, category_id, title) do
    %Thread{}
    |> Thread.changeset(%{category_id: category_id, title: title, user_id: user_id})
  end
end
