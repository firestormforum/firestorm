defmodule DataModelPlayground.Schema.UserTest do
  alias DataModelPlayground.{User, Repo}
  use ExUnit.Case
  import Ecto.Query

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "it requires a username" do
    changeset =
      %User{}
        |> User.changeset(%{})

    refute changeset.valid?
    assert changeset.errors[:username] == {"can't be blank", [validation: :required]}
  end

  test "it can be created" do
    {:ok, user} =
      %User{}
        |> User.changeset(%{username: "knewter"})
        |> Repo.insert

    assert user.id
  end
end
