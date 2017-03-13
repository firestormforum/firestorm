defmodule FirestormData.Commands.TagThread do
  @moduledoc """
  A command to tag a `Thread`.
  """

  use FirestormData.Command
  alias FirestormData.{Tagging, Tag, Thread}

  embedded_schema do
    field :tag_title, :string
    field :thread_id, :integer
  end

  @required_fields ~w(tag_title thread_id)a
  @optional_fields ~w()a

  def changeset(record, params \\ %{}) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  # Imagine we've extracted this to something like `Firestorm.run`
  # and it can handle all of our commands.
  #
  # For now we'll just put them in each command til I figure it out :)
  def run(changeset) do
    case changeset.valid? do
      true ->
        %{tag_title: tag_title, thread_id: thread_id} =
          changeset
          |> apply_changes

        tag_query =
          Tag
          |> where(title: ^tag_title)
          |> limit(1)

        case Repo.one(tag_query) do
          nil ->
            result =
              %Tag{}
              |> Tag.changeset(%{title: tag_title})
              |> Repo.insert

            case result do
              {:ok, tag} ->
                create_tagging(tag, thread_id, changeset)

              {:error, changes} ->
                {:error, Changeset.add_error(changeset, :tag_id, "There was an error", changes.errors)}
            end
          tag ->
            create_tagging(tag, thread_id, changeset)
        end

      false ->
        {:error, changeset}
    end
  end

  def handle_result({:ok, tagging}, _changeset) do
    {:ok, tagging.id}
  end
  def handle_result({:error, changes}, changeset) do
    # need to do better than this
    {:error, Changeset.add_error(changeset, :tag_id, "There was an error", changes.errors)}
  end

  def create_tagging(tag, thread_id, changeset) do
    thread = Repo.get(Thread, thread_id)

    thread
    |> Ecto.build_assoc(:taggings, %{tag_id: tag.id})
    |> Tagging.changeset(%{})
    |> Repo.insert
    |> handle_result(changeset)
  end
end
