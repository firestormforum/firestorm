defmodule DataModelPlayground.Schema.PostTest do
  alias DataModelPlayground.{Category, Thread, Post, Repo}
  use ExUnit.Case
  import Ecto.Query

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "it requires a thread_id" do
    changeset =
      %Post{}
        |> Post.changeset(%{})

    refute changeset.valid?
    assert changeset.errors[:thread_id] == {"can't be blank", [validation: :required]}
  end

  test "it requires a body" do
    changeset =
      %Post{}
        |> Post.changeset(%{})

    refute changeset.valid?
    assert changeset.errors[:body] == {"can't be blank", [validation: :required]}
  end

  test "it belongs to a category through its thread" do
    elixir =
      %Category{title: "Elixir"}
      |> Repo.insert!

    thread =
      %Thread{category_id: elixir.id, title: "ITT: Tests"}
      |> Repo.insert!

    attributes =
      %{
        thread_id: thread.id,
        body: "some body"
      }

    changeset =
      %Post{}
      |> Post.changeset(attributes)

    {:ok, saved_post} = Repo.insert(changeset)

    fetched_post =
      Post
      |> Repo.get(saved_post.id)
      |> Repo.preload([:thread, :category])

    assert fetched_post.thread.id == thread.id
    assert fetched_post.category.id == elixir.id
  end
end
