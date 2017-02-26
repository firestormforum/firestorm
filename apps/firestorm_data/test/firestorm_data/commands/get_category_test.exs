defmodule FirestormData.Commands.GetCategoryTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateCategory, GetCategory}
  alias FirestormData.{Category, Repo}

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
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "OTP"})

    {:ok, id} = CreateCategory.run(changeset)
    {:ok, %{category_id: id}}
  end
end
