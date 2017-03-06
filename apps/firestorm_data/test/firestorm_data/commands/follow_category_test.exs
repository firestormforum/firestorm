defmodule FirestormData.Commands.FollowCategoryTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateCategory, FollowCategory}
  alias FirestormData.{User, Repo, Followable, Category}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "following a category" do
    setup [:create_user, :create_category, :follow_category]

    test "returns expected results", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a follow in the database", %{category_id: category_id, user_id: user_id} do
      category = Repo.get(Category, category_id)
      user = Repo.get(User, user_id)
      assert Followable.followed_by?(category, user)
    end
  end

  def create_category(_) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "some title"})

    {:ok, category_id} = CreateCategory.run(changeset)

    {:ok, category_id: category_id}
  end

  def create_user(_) do
    changeset =
      %User{}
      |> User.changeset(%{username: "sonny"})

    {:ok, user} = Repo.insert(changeset)

    {:ok, user_id: user.id}
  end

  def follow_category(%{user_id: user_id, category_id: category_id}) do
    changeset =
      %FollowCategory{}
      |> FollowCategory.changeset(%{user_id: user_id, category_id: category_id})

    {:ok, result: FollowCategory.run(changeset)}
  end
end
