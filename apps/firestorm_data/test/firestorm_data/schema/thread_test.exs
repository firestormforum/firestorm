defmodule FirestormData.ThreadTest do
  alias FirestormData.{Category, Thread, Repo}
  import FirestormData.Factory
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    {:ok, category} = %Category{title: "Elixir"} |> Repo.insert
    {:ok, category: category}
  end

  test "creating a thread", %{category: category} do
    otp_changeset =
      %Thread{}
      |> Thread.changeset(%{category_id: category.id, title: "OTP is neat"})

    assert {:ok, _} = Repo.insert otp_changeset
  end


  test "finding the three threads with the most recent posts in a given category" do
    category = insert(:category)
    threads = insert_list(5, :thread, %{category: category})
    for thread <- threads do
      insert_list(3, :post, %{thread: thread})
    end
    [thread1, thread2, thread3, thread4, thread5] = threads
    :timer.sleep 1
    insert(:post, %{thread: thread1})
    insert(:post, %{thread: thread3})
    insert(:post, %{thread: thread5})

    recent_threads =
      category
      |> Thread.get_recent_threads
      |> Repo.all

    thread_ids =
      recent_threads
      |> Enum.map(&(&1.id))

    assert thread1.id in thread_ids
    assert thread3.id in thread_ids
    assert thread5.id in thread_ids
    refute thread2.id in thread_ids
    refute thread4.id in thread_ids
  end

  test "find threads a user has posted in" do
    user = insert(:user)
    [thread1, thread2, thread3] = insert_list(3, :thread)
    insert(:post, %{thread: thread1, user: user})
    insert(:post, %{thread: thread3, user: user})
    insert(:post, %{thread: thread2})

    user_thread_ids =
      user
      |> Thread.posted_in_by_user
      |> Repo.all
      |> Enum.map(&(&1.id))

    assert thread1.id in user_thread_ids
    assert thread3.id in user_thread_ids
    refute thread2.id in user_thread_ids
  end

  test "find out how many posts there are in a thread" do
    thread = insert(:thread)
    insert_list(5, :post, %{thread: thread})

    assert 5 = thread |> Thread.post_count |> Repo.one
  end
end
