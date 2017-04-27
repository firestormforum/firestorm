defmodule FirestormWeb.ForumsTest do
  use FirestormWeb.DataCase

  alias FirestormWeb.Forums
  alias FirestormWeb.Forums.User

  @create_attrs %{email: "some email", name: "some name", username: "some username"}
  @update_attrs %{email: "some updated email", name: "some updated name", username: "some updated username"}
  @invalid_attrs %{email: nil, name: nil, username: nil}

  def fixture(:user, attrs \\ @create_attrs) do
    {:ok, user} = Forums.create_user(attrs)
    user
  end

  test "list_users/1 returns all users" do
    user = fixture(:user)
    assert Forums.list_users() == [user]
  end

  test "get_user! returns the user with given id" do
    user = fixture(:user)
    assert Forums.get_user!(user.id) == user
  end

  test "create_user/1 with valid data creates a user" do
    assert {:ok, %User{} = user} = Forums.create_user(@create_attrs)
    assert user.email == "some email"
    assert user.name == "some name"
    assert user.username == "some username"
  end

  test "create_user/1 with invalid data returns error changeset" do
    assert {:error, %Ecto.Changeset{}} = Forums.create_user(@invalid_attrs)
  end

  test "update_user/2 with valid data updates the user" do
    user = fixture(:user)
    assert {:ok, user} = Forums.update_user(user, @update_attrs)
    assert %User{} = user
    assert user.email == "some updated email"
    assert user.name == "some updated name"
    assert user.username == "some updated username"
  end

  test "update_user/2 with invalid data returns error changeset" do
    user = fixture(:user)
    assert {:error, %Ecto.Changeset{}} = Forums.update_user(user, @invalid_attrs)
    assert user == Forums.get_user!(user.id)
  end

  test "delete_user/1 deletes the user" do
    user = fixture(:user)
    assert {:ok, %User{}} = Forums.delete_user(user)
    assert_raise Ecto.NoResultsError, fn -> Forums.get_user!(user.id) end
  end

  test "change_user/1 returns a user changeset" do
    user = fixture(:user)
    assert %Ecto.Changeset{} = Forums.change_user(user)
  end
end
