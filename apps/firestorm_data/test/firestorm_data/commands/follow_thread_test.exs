defmodule FirestormData.Commands.FollowThreadTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateCategory, CreateThread, FollowThread}
  alias FirestormData.{Thread, User, Repo, Followable}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "following a thread" do
    setup [:create_user, :create_category, :create_thread, :follow_thread]

    test "returns expected results", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a follow in the database", %{thread_id: thread_id, user_id: user_id} do
      thread = Repo.get(Thread, thread_id)
      user = Repo.get(User, user_id)
      assert Followable.followed_by?(thread, user)
    end
  end

  describe "when a thread is already followed" do
    setup [:create_user, :create_category, :create_thread, :follow_thread]

    test "following doesn't create a new record in the database", %{thread_id: thread_id, user_id: user_id} do
      thread = Repo.get(Thread, thread_id)
      assert Followable.follow_count(thread) == 1
      {:ok, result: result} = follow_thread(%{user_id: user_id, thread_id: thread_id})
      {:error, changeset} = result
      assert Followable.follow_count(thread) == 1
      assert changeset.errors[:user_id] == {"User already follows this thread", []}
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

  def create_thread(%{user_id: user_id, category_id: category_id}) do
    changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{user_id: user_id, title: "Some thread", body: "Some body", category_id: category_id})

    {:ok, thread_id} = CreateThread.run(changeset)

    {:ok, thread_id: thread_id}
  end

  def follow_thread(%{user_id: user_id, thread_id: thread_id}) do
    changeset =
      %FollowThread{}
      |> FollowThread.changeset(%{user_id: user_id, thread_id: thread_id})

    {:ok, result: FollowThread.run(changeset)}
  end
end
