defmodule FirestormData.Schema.ThreadTest do
  alias FirestormData.{
    Category,
    Thread,
    Repo,
    Post,
    User,
    Viewable,
    Followable
  }
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)

    bob =
      %User{username: "bob"}
      |> Repo.insert!

    {:ok, %{user: bob}}
  end

  test "it requires a category_id" do
    changeset =
      %Thread{}
        |> Thread.changeset(%{})

    refute changeset.valid?
    assert changeset.errors[:category_id] == {"can't be blank", [validation: :required]}
  end

  test "it requires a title" do
    changeset =
      %Thread{}
        |> Thread.changeset(%{})

    refute changeset.valid?
    assert changeset.errors[:title] == {"can't be blank", [validation: :required]}
  end

  test "it can have many views by users", %{user: user} do
    {:ok, {_elixir, tests_thread}} = create_category_and_thread("Elixir", "ITT: Tests")
    {:ok, _} = create_view(tests_thread, user)
    {:ok, _} = create_view(tests_thread, user)
    {:ok, _} = create_view(tests_thread, user)

    assert Viewable.view_count(tests_thread) == 3
  end

  test "it can be followed by users", %{user: user} do
    {:ok, {_elixir, tests_thread}} = create_category_and_thread("Elixir", "ITT: Tests")
    {:ok, _} = create_follow(tests_thread, user)

    assert Followable.followed_by?(tests_thread, user)
  end

  test "it knows whether it is fully read", %{user: user} do
    {:ok, {_elixir, tests_thread}} = create_category_and_thread("Elixir", "ITT: Tests")
    {:ok, _} = create_post(tests_thread, "knewter")
    tests_thread = tests_thread |> Repo.preload(:posts)
    [tests_thread_first_post|_] = tests_thread.posts

    refute Thread.completely_read?(tests_thread, user)
    {:ok, _} = create_view(tests_thread_first_post, user)
    assert Thread.completely_read?(tests_thread, user)
  end

  test "it belongs to a category" do
    {:ok, {elixir, saved_thread}} = create_category_and_thread("Elixir", "ITT: Tests")

    fetched_thread =
      Thread
      |> Repo.get(saved_thread.id)
      |> Repo.preload(:category)

    assert fetched_thread.category.id == elixir.id
  end

  test "it derives a user from the first post in the thread" do
    {:ok, {_elixir, saved_thread}} = create_category_and_thread("Elixir", "ITT: Tests")
    {:ok, _} = create_post(saved_thread, "knewter")
    :timer.sleep 10
    {:ok, _} = create_post(saved_thread, "adam")

    fetched_thread =
      Thread
      |> Repo.get(saved_thread.id)

    {:ok, user} = Thread.user(fetched_thread)
    assert user.username == "knewter"
  end

  defp create_category_and_thread(category_title, thread_title) do
    category =
      %Category{title: category_title}
      |> Repo.insert!

    attributes = %{
      category_id: category.id,
      title: thread_title
    }

    changeset =
      %Thread{}
        |> Thread.changeset(attributes)

    {:ok, saved_thread} = Repo.insert(changeset)
    {:ok, {category, saved_thread}}
  end

  defp create_post(thread, username) do
    user = Repo.insert!(%User{username: username})

    %Post{}
      |> Post.changeset(%{user_id: user.id, body: "foo", thread_id: thread.id})
      |> Repo.insert
  end

  defp create_view(thread, user) do
    {:ok, _} =
      thread
      |> Ecto.build_assoc(:views, %{user_id: user.id})
      |> Repo.insert
  end

  defp create_follow(thread, user) do
    thread
    |> Ecto.build_assoc(:follows, %{user_id: user.id})
    |> Repo.insert
  end
end
