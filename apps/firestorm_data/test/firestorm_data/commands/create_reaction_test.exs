defmodule FirestormData.Commands.CreateReactionTest do
  @moduledoc false

  use FirestormData.UnitCase
  alias FirestormData.Commands.{CreateCategory, CreateThread, CreateReaction}
  alias FirestormData.{Thread, User, Repo, Post, Reaction}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "reacting to a post" do
    setup [:create_user, :create_category, :create_thread, :react_to_post]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a reaction in the database", %{post_id: post_id} do
      post =
        Post
        |> Repo.get(post_id)
        |> Repo.preload(:reactions)
      assert 1 == length(post.reactions)
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
      |> CreateThread.changeset(%{
           user_id: user_id,
           title: "Some thread",
           body: "Some body",
           category_id: category_id
         })

    {:ok, thread_id} = CreateThread.run(changeset)

    {:ok, thread_id: thread_id}
  end

  def react_to_post(%{user_id: user_id, thread_id: thread_id}) do
    require Ecto.Query
    thread =
      Thread
      |> Ecto.Query.preload(:posts)
      |> Ecto.Query.preload(:category)
      |> Repo.get(thread_id)
    [fp|_] = thread.posts

    changeset =
      %CreateReaction{}
      |> CreateReaction.changeset(%{user_id: user_id, post_id: fp.id, emoji: "thumbsup"})

    {:ok, post_id: fp.id, result: CreateReaction.run(changeset)}
  end
end
