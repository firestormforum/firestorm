defmodule FirestormData.Category do
  use Ecto.Schema
  import Ecto.Changeset

  schema "categories" do
    field :title, :string

    timestamps()
  end

  def changeset(category, params \\ %{}) do
    category
    |> cast(params, [:title])
  end

  # Queries - we should move these out of the schema, but we'll put them here
  # for now for illustrative purposes.
  #
  # - In general, I don't want my schema to know about the Repo at all
  # - Also I don't want my Category module having a function that returns Threads
  # I don't think...
  def get_recent_threads(category) do
    alias FirestormData.Thread
    import Ecto.Query
    Thread
      |> join(:left_lateral, [t], p in fragment("SELECT thread_id, inserted_at FROM posts WHERE posts.thread_id = ? ORDER BY posts.inserted_at DESC LIMIT 1", t.id))
      |> order_by([t, p], [desc: p.inserted_at])
      |> select([t], t)
      |> limit(3)
  end
end
