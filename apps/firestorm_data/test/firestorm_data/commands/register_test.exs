defmodule FirestormData.Commands.RegisterTest do
  use ExUnit.Case
  alias FirestormData.Commands.Register
  alias FirestormData.{User, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "registering a username that doesn't exist" do
    setup [:register_user]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a user in the database", %{result: {:ok, user_id}} do
      assert Repo.get(User, user_id)
    end
  end

  describe "registering a username that already exists" do
    setup [:register_user, :register_user]

    test "returns a username taken error", %{result: result} do
      assert {:error, [username: {"has already been taken", []}]} = result
    end
  end

  def register_user(_) do
    options =
      %Register{
        username: "josh",
        password: "secret"
      }

    result = Register.run(options)
    {:ok, %{result: result}}
  end
end
