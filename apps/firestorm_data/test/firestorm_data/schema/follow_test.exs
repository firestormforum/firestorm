defmodule FirestormData.Schema.FollowTest do
  alias FirestormData.{Follow, Repo}
  use ExUnit.Case

  @valid_attributes %{
    user_id: 1,
    assoc_id: 2
  }

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "it requires an user_id" do
    changeset =
      %Follow{}
        |> Follow.changeset(Map.delete(@valid_attributes, :user_id))

    refute changeset.valid?
    assert changeset.errors[:user_id] == {"can't be blank", [validation: :required]}
  end

  test "it requires an assoc_id" do
    changeset =
      %Follow{}
        |> Follow.changeset(Map.delete(@valid_attributes, :assoc_id))

    refute changeset.valid?
    assert changeset.errors[:assoc_id] == {"can't be blank", [validation: :required]}
  end
end
