# NOTE: Trying to start pretending we have a Context now and abandoning
# Commands...this should be a query anyway!
defmodule FirestormData.Commands.GetFollowedThreadsTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateCategory, CreateThread, FollowThread}
  alias FirestormData.{Repo, Thread, User, Category}
  alias FirestormData.Forums

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo, ownership_timeout: 3_000_000)
  end

  describe "getting all followed threads" do
    setup [:create_user, :create_category, :create_threads, :follow_threads]

    test "finds threads that are followed",
      %{
        user_id: user_id,
        thread1_id: thread1_id
      }  do

      user = Forums.get_user!(user_id)

      thread_ids =
        Forums.get_followed_threads!(user)
        |> Enum.map(&(&1.id))

      assert thread1_id in thread_ids
    end
  end

  def create_category(_) do
    elixir_changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "Elixir"})

    {:ok, elixir_id} = CreateCategory.run(elixir_changeset)

    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "OTP", parent_id: elixir_id})

    {:ok, id} = CreateCategory.run(changeset)
    {:ok, %{category_id: id}}
  end

  def create_threads(%{user_id: user_id, category_id: category_id}) do
    thread1_changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{
        title: "OTP",
        body: "body",
        category_id: category_id,
        user_id: user_id
      })
    thread2_changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{
        title: "GenServer",
        body: "body",
        category_id: category_id,
        user_id: user_id
      })

    {:ok, thread1_id} = CreateThread.run(thread1_changeset)
    {:ok, thread2_id} = CreateThread.run(thread2_changeset)
    {:ok, %{thread1_id: thread1_id, thread2_id: thread2_id}}
  end

  def follow_threads(%{user_id: user_id, thread1_id: thread1_id}) do
    %FollowThread{}
    |> FollowThread.changeset(%{user_id: user_id, thread_id: thread1_id})
    |> FollowThread.run

    :ok
  end

  def create_user(_) do
    changeset =
      %User{}
      |> User.changeset(%{username: "sonny", email: "sonny@example.com"})

    {:ok, user} = Repo.insert(changeset)
    {:ok, %{user_id: user.id}}
  end
end
