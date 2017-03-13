defmodule FirestormData.Commands.UnfollowThreadTest do
  use FirestormData.UnitCase
  alias FirestormData.Commands.{CreateCategory, CreateThread, FollowThread, UnfollowThread}
  alias FirestormData.{Thread, User, Repo, Followable}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "Unfollowing a thread" do
    setup [:create_user, :create_category, :create_thread, :follow_thread, :unfollow_thread]

    test "returns expected result", %{result: result} do
      assert :ok = result
    end

    test "deletes the follow from the database", %{thread_id: thread_id, user_id: user_id} do
      thread = Repo.get(Thread, thread_id)
      user = Repo.get(User, user_id)
      refute Followable.followed_by?(thread, user)
    end
  end

  describe "when a thread is not followed" do
    setup [:create_user, :create_category, :create_thread, :unfollow_thread]

    test "unfollowing does nothing and pretends to have worked", %{thread_id: thread_id, user_id: user_id} do
      thread = Repo.get(Thread, thread_id)
      assert Followable.follow_count(thread) == 0
      {:ok, result: result} = unfollow_thread(%{user_id: user_id, thread_id: thread_id})
      assert :ok = result
      assert Followable.follow_count(thread) == 0
    end
  end

  def create_category(_) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "some title"})

    {:ok, category_id} = CreateCategory.run(changeset)

    {:ok, category_id: category_id}
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
    {:ok, follow_id} = FollowThread.run(changeset)

    {:ok, follow_id: follow_id}
  end

  def unfollow_thread(%{user_id: user_id, thread_id: thread_id}) do
    changeset =
      %UnfollowThread{}
      |> UnfollowThread.changeset(%{user_id: user_id, thread_id: thread_id})
    result = UnfollowThread.run(changeset)

    {:ok, result: result}
  end
end
