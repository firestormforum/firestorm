defmodule FirestormWeb.Forums.Notification do
  @moduledoc """
  Schema for notifications.
  """

  use Ecto.Schema
  alias FirestormWeb.Forums.{User, Thread, View}

  schema "forums_notifications" do
    field :body, :string
    belongs_to :user, User

    timestamps()
  end
end
