defmodule FirestormData.Commands.ViewThreadTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateCategory, CreateThread, ViewThread}
  alias FirestormData.{Thread, User, Repo, Viewable}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "viewing a thread" do
    setup [:create_user, :create_category, :create_thread, :view_thread]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a view in the database", %{thread_id: thread_id} do
      thread = Repo.get(Thread, thread_id)
      assert 1 == Viewable.view_count(thread)
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

  def view_thread(%{user_id: user_id, thread_id: thread_id}) do
    changeset =
      %ViewThread{}
      |> ViewThread.changeset(%{user_id: user_id, thread_id: thread_id})

    {:ok, result: ViewThread.run(changeset)}
  end
end
