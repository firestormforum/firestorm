defmodule DataModelPlayground.Commands.CreateCategoryTest do
  use ExUnit.Case
  alias DataModelPlayground.Commands.CreateCategory
  alias DataModelPlayground.{Category, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "creating a category" do
    setup [:create_category]

    test "returns expected results", %{result: result} do
      assert {:ok, _some_id} = result
    end

    test "creates a category in the database", %{result: {:ok, category_id}} do
      assert Repo.get(Category, category_id)
    end
  end

  def create_category(_) do
    options =
      %CreateCategory{
        title: "some title"
      }

    result = CreateCategory.run(options)
    {:ok, %{ result: result }}
  end
end
