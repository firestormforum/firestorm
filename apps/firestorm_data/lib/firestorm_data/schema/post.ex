defmodule FirestormData.Post do
  @moduledoc """
  A `Post` is a single piece of communication within a `Thread`
  """

  use Ecto.Schema
  import Ecto.Changeset
  alias FirestormData.{
    Thread,
    User,
    View,
    Repo,
    Reaction
  }

  schema "posts" do
    belongs_to :thread, Thread
    has_one :category, through: [:thread, :category]
    belongs_to :user, User
    field :body, :string
    has_many :views, {"posts_views", View}, foreign_key: :assoc_id
    has_many :reactions, Reaction

    timestamps()
  end

  @required_fields ~w(thread_id body user_id)a
  @optional_fields ~w()a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end
