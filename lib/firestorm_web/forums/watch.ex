defmodule FirestormWeb.Forums.Watch do
  @moduledoc """
  A `Watch` is a polymorphic representation that a user is watching a thing in our system.
  """

  use Ecto.Schema

  schema "abstract table: watches" do
    # This will be used by associations on each "concrete" table
    field :assoc_id, :integer
    field :user_id, :integer

    timestamps()
  end
end
