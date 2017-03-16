defmodule FirestormData.Commands.LoginTest do
  use FirestormData.UnitCase
  alias FirestormData.Commands.{Login, Register}
  alias FirestormData.{Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "logging in a user that exists" do
    setup [:register_user]

    test "succeeds with the right password", %{user_id: user_id}  do
      options =
        %Login{
          username: "josh",
          password: "secret"
        }

      assert {:ok, ^user_id} = Login.run(options)
    end

    test "fails with the wrong password" do
      options =
        %Login{
          username: "josh",
          password: "wrong"
        }

      assert {:error, :invalid_credentials} = Login.run(options)
    end
  end

  def register_user(_) do
    options =
      %Register{
        username: "josh",
        email: "josh@example.org",
        name: "Josh Adams",
        password: "secret"
      }

    {:ok, user_id} = Register.run(options)
    {:ok, %{user_id: user_id}}
  end
end
