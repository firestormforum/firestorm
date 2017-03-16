defmodule FirestormData.Commands.CreateThreadTest do
  use FirestormData.UnitCase
  alias FirestormData.Commands.{CreateThread, CreateCategory}
  alias FirestormData.{User, Repo, Thread}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "creating a thread" do
    setup [:create_user, :create_category, :create_thread]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a thread in the database", %{result: {:ok, thread_id}} do
      thread =
        Thread
        |> Repo.get(thread_id)
        |> Repo.preload([:posts])

      assert thread
      assert length(thread.posts) == 1
    end
  end

  def create_category(_) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "some title"})

    {:ok, category_id} = CreateCategory.run(changeset)
    {:ok, %{category_id: category_id}}
  end

  def create_thread(%{user_id: user_id, category_id: category_id}) do
    changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{user_id: user_id, title: "Some thread", body: "Some body", category_id: category_id})

    {:ok, result: CreateThread.run(changeset)}
  end
end
