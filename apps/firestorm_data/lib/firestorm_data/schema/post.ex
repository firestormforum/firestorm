defmodule FirestormData.Post do
  use Ecto.Schema
  import Ecto.Changeset
  alias FirestormData.{Repo, Category, Thread, User}

  schema "posts" do
    belongs_to :thread, Thread
    has_one :category, through: [:thread, :category]
    belongs_to :user, User
    field :body, :string

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
