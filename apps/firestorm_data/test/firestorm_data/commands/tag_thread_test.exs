defmodule FirestormData.Commands.TagThreadTest do
  use FirestormData.UnitCase
  alias FirestormData.Commands.{CreateCategory, CreateThread, TagThread}
  alias FirestormData.{Thread, User, Repo, Tag, Taggable}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "tagging a thread" do
    setup [:create_user, :create_category, :create_tag, :create_thread, :tag_thread]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a tagging in the database", %{thread_id: thread_id, tag_id: tag_id} do
      thread = Repo.get(Thread, thread_id)
      tag = Repo.get(Tag, tag_id)
      assert Taggable.tagged_with?(thread, tag)
    end
  end

  def create_category(_) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "some title"})

    {:ok, category_id} = CreateCategory.run(changeset)

    {:ok, category_id: category_id}
  end

  def create_tag(_) do
    title = "phoenix"

    changeset =
      %Tag{}
      |> Tag.changeset(%{title: title})

    {:ok, tag} = Repo.insert(changeset)

    {:ok, tag_id: tag.id, tag_title: title}
  end

  def create_thread(%{user_id: user_id, category_id: category_id}) do
    changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{user_id: user_id, title: "Some thread", body: "Some body", category_id: category_id})

    {:ok, thread_id} = CreateThread.run(changeset)

    {:ok, thread_id: thread_id}
  end

  def tag_thread(%{tag_id: tag_id, thread_id: thread_id}) do
    changeset =
      %TagThread{}
      |> TagThread.changeset(%{tag_id: tag_id, thread_id: thread_id})

    {:ok, result: TagThread.run(changeset)}
  end
end
