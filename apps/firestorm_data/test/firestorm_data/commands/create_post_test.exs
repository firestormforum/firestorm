defmodule FirestormData.Commands.CreatePostTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreatePost, CreateThread, CreateCategory}
  alias FirestormData.{User, Repo, Post}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "creating a post" do
    setup [:create_user, :create_category, :create_thread, :create_post]

    test "returns expected results", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a post in the database", %{result: {:ok, post_id}} do
      post =
        Post
        |> Repo.get(post_id)

      assert post
    end
  end

  def create_category(_) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "some title"})

    {:ok, category_id} = CreateCategory.run(changeset)
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

    {:ok, thread_id} = CreateThread.run(changeset)

    {:ok, thread_id: thread_id}
  end

  def create_post(%{user_id: user_id, thread_id: thread_id}) do
    changeset =
      %CreatePost{}
      |> CreatePost.changeset(%{user_id: user_id, body: "Some body", thread_id: thread_id})

    {:ok, result: CreatePost.run(changeset)}
  end
end
