defmodule FirestormData.Reaction do
  @moduledoc """
  A `Reaction` is an emoji by a `User` on a `Post`
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "reactions" do
    belongs_to :user, User
    belongs_to :post, Post
    field :emoji, :string

    timestamps()
  end

  @required_fields ~w(user_id emoji post_id)a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
  end
end
