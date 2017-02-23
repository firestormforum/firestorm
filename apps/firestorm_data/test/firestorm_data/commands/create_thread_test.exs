defmodule FirestormData.Commands.CreateThreadTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateThread, CreateCategory}
  alias FirestormData.{Category, User, Repo, Thread}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "creating a thread" do
    setup [:create_user, :create_category, :create_thread]

    test "returns expected results", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a thread in the database", %{result: {:ok, thread_id}} do
      thread =
        Repo.get(Thread, thread_id)
        |> Repo.preload([:posts])

      assert thread
      assert length(thread.posts) == 1
    end
  end

  def create_category(_) do
    options =
      %CreateCategory{
        title: "some title"
      }

    {:ok, category_id} = CreateCategory.run(options)
    {:ok, %{category_id: category_id}}
  end

  def create_user(_) do
    changeset =
      %User{}
      |> User.changeset(%{username: "sonny"})

    {:ok, user} = Repo.insert(changeset)
    {:ok, %{user_id: user.id}}
  end

  def create_thread(%{user_id: user_id, category_id: category_id}) do
    changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{user_id: user_id, title: "Some thread", body: "Some body", category_id: category_id})

    {:ok, result: CreateThread.run(changeset)}
  end
end
