defmodule FirestormWeb.Forums.Post do
  use Ecto.Schema
  alias FirestormWeb.Forums.{User, Thread}

  schema "forums_posts" do
    field :body, :string
    belongs_to :thread, Thread
    belongs_to :user, User

    timestamps()
  end
end
