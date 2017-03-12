defmodule FirestormData.Schema.TaggingTest do
  alias FirestormData.{Tagging, Repo}
  use ExUnit.Case

  @valid_attributes %{
    tag_id: 1,
    assoc_id: 2
  }

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "it requires a tag_id" do
    changeset =
      %Tagging{}
        |> Tagging.thread_changeset(Map.delete(@valid_attributes, :tag_id))

    refute changeset.valid?
    assert changeset.errors[:tag_id] == {"can't be blank", [validation: :required]}
  end

  test "it requires an assoc_id" do
    changeset =
      %Tagging{}
        |> Tagging.thread_changeset(Map.delete(@valid_attributes, :assoc_id))

    refute changeset.valid?
    assert changeset.errors[:assoc_id] == {"can't be blank", [validation: :required]}
  end
end
