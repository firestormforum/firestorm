defmodule FirestormData.Commands.CreateCategoryTest do
  use ExUnit.Case
  alias FirestormData.Commands.CreateCategory
  alias FirestormData.{Category, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "creating a category" do
    setup [:create_category]

    test "returns expected result", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a category in the database", %{result: {:ok, category_id}} do
      assert Repo.get(Category, category_id)
    end
  end

  def create_category(_) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "some title"})

    result = CreateCategory.run(changeset)
    {:ok, %{result: result}}
  end
end
