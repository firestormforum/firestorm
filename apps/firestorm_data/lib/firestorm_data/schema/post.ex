defmodule FirestormData.Post do
  use Ecto.Schema
  import Ecto.Changeset

  schema "posts" do
    field :body, :string
    belongs_to :thread, FirestormData.Thread
    belongs_to :user, FirestormData.User

    timestamps()
  end

  def changeset(post, params \\ %{}) do
    post
    |> cast(params, [:body, :thread_id, :user_id])
  end
end
