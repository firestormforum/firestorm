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
      |> Category.get_recent_threads
      |> Repo.all

    IO.inspect recent_threads

    thread_ids =
      recent_threads
      |> Enum.map(&(&1.id))

    assert thread1.id in thread_ids
    assert thread3.id in thread_ids
    assert thread5.id in thread_ids
    refute thread2.id in thread_ids
    refute thread4.id in thread_ids
  end
end
