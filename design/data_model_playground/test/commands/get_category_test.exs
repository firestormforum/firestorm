defmodule DataModelPlayground.Commands.GetCategoryTest do
  use ExUnit.Case
  alias DataModelPlayground.Commands.{CreateCategory, GetCategory}
  alias DataModelPlayground.{Category, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "getting a category" do
    setup [:create_category]

    test "works via id",
      %{
        category_id: category_id
      }  do

      options =
        %GetCategory{
          finder: category_id
        }

      assert {:ok, %{id: category_id}} = GetCategory.run(options)
    end

    test "works via slug",
      %{
        category_id: category_id
      }  do

      category = Repo.get(Category, category_id)
      options =
        %GetCategory{
          finder: category.slug
        }

      assert {:ok, %{id: category_id}} = GetCategory.run(options)
    end
  end

  def create_category(_) do
    options =
      %CreateCategory{
        title: "OTP",
      }

    {:ok, id} = CreateCategory.run(options)
    {:ok, %{category_id: id}}
  end
end
