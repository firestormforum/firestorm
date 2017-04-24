defmodule FirestormData.ReactionTest do
  use ExUnit.Case
  alias FirestormData.{
    Reaction,
    Repo,
    User,
    Category,
    Thread,
    Post
  }

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)

    {:ok, user} =
      %User{}
      |> User.changeset(%{username: "sonny", email: "sonny@example.org"})
      |> Repo.insert

    {:ok, category} =
      %Category{}
      |> Category.changeset(%{title: "foo"})
      |> Repo.insert

    {:ok, thread} =
      %Thread{}
      |> Thread.changeset(%{category_id: category.id, user_id: user.id, title: "bar"})
      |> Repo.insert

    {:ok, post} =
      %Post{}
      |> Post.changeset(%{thread_id: thread.id, user_id: user.id, body: "body"})
      |> Repo.insert


    {:ok, %{user: user, thread: thread, category: category, post: post}}
  end

  test "it requires a post_id" do
    changeset =
      %Reaction{}
      |> Reaction.changeset(%{emoji: "thumbsup", user_id: 1})

    refute changeset.valid?
    assert changeset.errors[:post_id] == {"can't be blank", [validation: :required]}
  end

  test "it requires a user_id" do
    changeset =
      %Reaction{}
      |> Reaction.changeset(%{emoji: "thumbsup", post_id: 1})

    refute changeset.valid?
    assert changeset.errors[:user_id] == {"can't be blank", [validation: :required]}
  end

  test "it requires an emoji" do
    changeset =
      %Reaction{}
      |> Reaction.changeset(%{user_id: 1, post_id: 1})

    refute changeset.valid?
    assert changeset.errors[:emoji] == {"can't be blank", [validation: :required]}
  end

  test "insertion works", %{user: user, post: post} do
    {:ok, reaction} =
      %Reaction{}
      |> Reaction.changeset(%{user_id: user.id, post_id: post.id, emoji: "thumbsub"})
      |> Repo.insert

    assert Repo.get(Reaction, reaction.id)
  end
end
