defmodule FirestormData.Post do
  @moduledoc """
  A `Post` is a single piece of communication within a `Thread`
  """

  use Ecto.Schema
  import Ecto.Changeset
  alias FirestormData.{Thread, User, View, Repo}

  schema "posts" do
    belongs_to :thread, Thread
    has_one :category, through: [:thread, :category]
    belongs_to :user, User
    field :body, :string
    has_many :views, {"posts_views", View}, foreign_key: :assoc_id

    timestamps()
  end

  @required_fields ~w(thread_id body user_id)a
  @optional_fields ~w()a

  def changeset(record, params \\ :empty) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def view_count(post) do
    post
    |> Ecto.assoc(:views)
    |> Repo.aggregate(:count, :id)
  end
end
