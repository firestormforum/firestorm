defmodule FirestormData.PostTest do
  alias FirestormData.{Category, User, Thread, Post, Repo}
  use ExUnit.Case
  import Ecto.Query

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    {:ok, category} = %Category{title: "Elixir"} |> Repo.insert
    {:ok, otp} = %Thread{title: "OTP is neat", category_id: category.id} |> Repo.insert
    {:ok, josh} = %User{username: "josh", email: "josh@dailydrip.com", name: "Josh Adams"} |> Repo.insert
    {:ok, category: category, otp: otp, josh: josh}
  end

  test "creating a post", %{otp: otp, josh: josh} do
    post_changeset =
      %Post{}
      |> Post.changeset(%{thread_id: otp.id, body: "I know, right?", user_id: josh.id})

    assert {:ok, _} = Repo.insert post_changeset
  end

  describe "given some posts" do
    setup [:create_other_users, :create_sample_posts]

    test "finding a post by a user", %{post1: post1, josh: josh} do
      query =
        from p in Post,
          where: p.user_id == ^josh.id

      posts = Repo.all query
      assert post1.id in Enum.map(posts, &(&1.id))
      post1 = post1 |> Repo.preload([:user])
      assert post1.user.username == "josh"
    end

    test "counting the posts in a thread", %{otp: otp} do
      query =
        from p in Post,
          where: p.thread_id == ^otp.id

      posts_count = Repo.aggregate(query, :count, :id)
      assert posts_count == 2
    end
  end

  defp create_other_users(_) do
    adam =
      %User{username: "adam", email: "adam@dailydrip.com", name: "Adam Dill"}
      |> Repo.insert!

    {:ok, adam: adam}
  end

  defp create_sample_posts(%{otp: otp, josh: josh, adam: adam}) do
    post1 =
      %Post{}
      |> Post.changeset(%{thread_id: otp.id, user_id: josh.id, body: "a"})
      |> Repo.insert!

    post2 =
      %Post{}
      |> Post.changeset(%{thread_id: otp.id, user_id: adam.id, body: "b"})
      |> Repo.insert!

    {:ok, post1: post1, post2: post2}
  end
end
