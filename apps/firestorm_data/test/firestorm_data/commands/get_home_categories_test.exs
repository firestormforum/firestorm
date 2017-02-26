defmodule FirestormData.Commands.GetHomeCategoriesTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateCategory, GetHomeCategories}
  alias FirestormData.{Category, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "fetching categories for the home screen" do
    setup [:create_categories, :get_home_categories]

    test "returns root categories",
      %{
        category_ids: category_ids,
        result: result
      }  do

      {:ok, categories} = result

      result_ids =
        categories
        |> Enum.map(&(&1.id))

      assert Enum.sort(category_ids) == Enum.sort(result_ids)
    end
  end

  def create_categories(_) do
    changeset_foo =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "foo"})
    changeset_bar =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "bar"})

    {:ok, foo_id} = CreateCategory.run(changeset_foo)
    {:ok, bar_id} = CreateCategory.run(changeset_bar)

    changeset_baz =
      %CreateCategory{}
      |> CreateCategory.changeset(%{
        title: "baz",
        parent_id: bar_id
      })

    {:ok, _} = CreateCategory.run(changeset_baz)

    {:ok, %{category_ids: [foo_id, bar_id]}}
  end

  def get_home_categories(_) do
    changeset =
      %GetHomeCategories{user_id: 0}

    result = GetHomeCategories.run(changeset)
    {:ok, result: result}
  end
end
