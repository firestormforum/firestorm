defmodule FirestormWeb.Forums.View do
  @moduledoc """
  A `View` is a polymorphic representation that a user viewed a thing in our
  system at a specified time.
  """

  use Ecto.Schema

  schema "abstract table: views" do
    # This will be used by associations on each "concrete" table
    field :assoc_id, :integer
    field :user_id, :integer

    timestamps()
  end
end

