defmodule FirestormData.Commands.LoginOrRegisterFromGitHubTest do
  use FirestormData.UnitCase
  alias FirestormData.Commands.{LoginOrRegisterFromGitHub}
  alias FirestormData.{User, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "registering a new user" do
    setup [:register]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a user in the database", %{result: {:ok, user}} do
      assert user.email == "knewter@example.org"
    end
  end

  def register(_) do
    changeset =
      %LoginOrRegisterFromGitHub{}
      |> LoginOrRegisterFromGitHub.changeset(%{email: "knewter@example.org", username: "knewter", name: "Josh Adams"})

    {:ok, result: LoginOrRegisterFromGitHub.run(changeset)}
  end
end
