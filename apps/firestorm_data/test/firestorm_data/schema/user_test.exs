defmodule FirestormData.UserTest do
  alias FirestormData.{User, Repo}
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "creating a user" do
    josh_changeset =
      %User{}
      |> User.changeset(%{name: "Josh Adams", email: "josh@dailydrip.com"})

    assert {:ok, _} = Repo.insert josh_changeset
  end

  test "creating a user without an email" do
    josh_changeset =
      %User{}
      |> User.changeset(%{name: "Josh Adams"})

    assert {:email, {"can't be blank", [validation: :required]}} in josh_changeset.errors
  end

  test "creating a user with an invalid email" do
    josh_changeset =
      %User{}
      |> User.changeset(%{name: "Josh Adams", email: "notanemail"})

    refute josh_changeset.valid?
  end

  test "creating two users with the same email address" do
    josh_changeset =
      %User{}
      |> User.changeset(%{name: "Josh Adams", email: "josh@dailydrip.com"})

    assert {:ok, _} = Repo.insert(josh_changeset)
    {:error, new_changeset} = Repo.insert(josh_changeset)
    assert {:email, {"has already been taken", []}} in new_changeset.errors
  end
end
