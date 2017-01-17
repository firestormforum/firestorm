defmodule DataModelPlayground.Schema.CategoryTest do
  alias DataModelPlayground.Category
  use ExUnit.Case
  alias DataModelPlayground.Repo

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "adding and retrieving categories" do
    # NOTE: We will not be doing repo stuff in the modules but i'm in a hurry
    assert [] == Category.categories

    Category.add("OTP")
    Category.add("Phoenix")

    assert [%{title: "OTP"}, %{title: "Phoenix"}] = Category.categories
  end
end
