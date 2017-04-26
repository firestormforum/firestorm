defmodule FirestormData.Thread do
  use Ecto.Schema
  import Ecto.Changeset

  schema "threads" do
    field :title, :string
    belongs_to :category, FirestormData.Category

    timestamps()
  end

  def changeset(thread, params \\ %{}) do
    thread
    |> cast(params, [:category_id, :title])
  end

  # Queries - we should move these out of the schema, but we'll put them here
  # for now for illustrative purposes.
  def get_recent_threads(category) do
    import Ecto.Query

    __MODULE__
      |> join(:left_lateral, [t], p in fragment("SELECT thread_id, inserted_at FROM posts WHERE posts.thread_id = ? ORDER BY posts.inserted_at DESC LIMIT 1", t.id))
      |> order_by([t, p], [desc: p.inserted_at])
      |> where(category_id: ^category.id)
      |> select([t], t)
      |> limit(3)
  end

  def posted_in_by_user(user) do
    import Ecto.Query
    alias FirestormData.{Post, Thread}

    Post
      |> where([p], p.user_id == ^user.id)
      |> join(:inner, [p], t in Thread, p.thread_id == t.id)
      |> select([p, t], t)
  end

  def post_count(thread) do
    import Ecto.Query
    alias FirestormData.Post

    Post
      |> where([p], p.thread_id == ^thread.id)
      |> group_by([p], p.thread_id)
      |> select([p], count(p.id))
  end
end
