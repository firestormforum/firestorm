defmodule FirestormData.Commands.ViewPostTest do
  @moduledoc false

  use FirestormData.UnitCase
  alias FirestormData.Commands.{CreateCategory, CreateThread, ViewPost}
  alias FirestormData.{Thread, User, Repo, Post, Viewable}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "viewing a post" do
    setup [:create_user, :create_category, :create_thread, :view_post]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a view in the database", %{post_id: post_id} do
      post = Repo.get(Post, post_id)
      assert 1 == Viewable.view_count(post)
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

  def view_post(%{user_id: user_id, thread_id: thread_id}) do
    require Ecto.Query
    thread =
      Thread
      |> Ecto.Query.preload(:posts)
    # NOTE: If you uncomment the next line, we see no posts. Interesting bug eh?
    #|> Ecto.Query.preload(:category)
      |> Repo.get(thread_id)
    [fp|_] = thread.posts

    changeset =
      %ViewPost{}
      |> ViewPost.changeset(%{user_id: user_id, post_id: fp.id})

    {:ok, post_id: fp.id, result: ViewPost.run(changeset)}
  end
end
