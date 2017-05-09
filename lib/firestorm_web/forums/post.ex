defmodule FirestormWeb.Forums.Post do
  use Ecto.Schema

  schema "forums_posts" do
    field :body, :string
    field :thread_id, :id
    field :user_id, :id

    timestamps()
  end
end
