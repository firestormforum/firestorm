defmodule DataModelPlayground.Commands.GetHomeCategoriesTest do
  use ExUnit.Case
  alias DataModelPlayground.Commands.{CreateCategory, GetHomeCategories}
  alias DataModelPlayground.{Category, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "fetching categories for the home screen" do
    setup [:create_categories, :get_home_categories]

    # FIXME: Don't return all!
    test "returns all categories",
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
    options_foo =
      %CreateCategory{
        title: "foo",
      }
    options_bar =
      %CreateCategory{
        title: "bar",
      }

    {:ok, foo_id} = CreateCategory.run(options_foo)
    {:ok, bar_id} = CreateCategory.run(options_bar)
    {:ok, %{category_ids: [foo_id, bar_id]}}
  end

  def get_home_categories(_) do
    options =
      %GetHomeCategories{user_id: 0}

    result = GetHomeCategories.run(options)
    {:ok, result: result}
  end
end
