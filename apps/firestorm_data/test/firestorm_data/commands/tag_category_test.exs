defmodule FirestormData.Commands.TagCategoryTest do
  use FirestormData.UnitCase
  alias FirestormData.Commands.{CreateCategory, TagCategory}
  alias FirestormData.{Category, User, Repo, Tag, Taggable, Tagging}
  import Ecto.Query
  @tag_title "phoenix"

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "tagging a category" do
    setup [:create_user, :create_category, :tag_category]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a tagging in the database", %{category_id: category_id} do
      category = Repo.get(Category, category_id)

      tag =
        Tag
        |> where(title: ^@tag_title)
        |> limit(1)
        |> Repo.one

      assert Taggable.tagged_with?(category, tag)
    end
  end

  describe "tagging two categories with a tag that generates the same slug" do
    setup [:create_user, :create_category, :tag_category]

    test "creating a second category and tagging it with a tag that generates the same slug as an existing tag", %{category_id: category_id} do
      changeset =
        %CreateCategory{}
        |> CreateCategory.changeset(%{title: "second category"})

      {:ok, second_category_id} = CreateCategory.run(changeset)

      changeset =
        %TagCategory{}
        |> TagCategory.changeset(%{tag_title: ":#{@tag_title}", category_id: second_category_id})

      assert {:ok, _tagging_id} = TagCategory.run(changeset)
    end
  end

  def create_category(_) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "some title"})

    {:ok, category_id} = CreateCategory.run(changeset)

    {:ok, category_id: category_id}
  end

  def tag_category(%{category_id: category_id}) do
    changeset =
      %TagCategory{}
      |> TagCategory.changeset(%{tag_title: @tag_title, category_id: category_id})

    {:ok, result: TagCategory.run(changeset)}
  end
end
