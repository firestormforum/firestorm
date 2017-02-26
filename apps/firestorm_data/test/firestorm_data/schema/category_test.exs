defmodule FirestormData.Schema.CategoryTest do
  alias FirestormData.{Category, Repo}
  use ExUnit.Case
  @valid_attributes %{
    title: "Something"
  }

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "it requires a title" do
    changeset =
      %Category{}
        |> Category.changeset(Map.delete(@valid_attributes, :title))

    refute changeset.valid?
    assert changeset.errors[:title] == {"can't be blank", [validation: :required]}
  end

  test "it generates a slug" do
    Category.add("OTP")

    assert Repo.one(Category).slug == "otp"
  end

  test "adding and retrieving categories" do
    # NOTE: We will not be doing repo stuff in the modules but i'm in a hurry
    assert [] == Category.categories

    Category.add("OTP")
    Category.add("Phoenix")

    assert [%{title: "OTP"}, %{title: "Phoenix"}] = Category.categories
  end

  test "tree structure" do
    elixir = make_elixir_otp_tree()

    children =
      elixir
        |> Category.children
        |> Repo.all

    assert length(children) == 1
    assert hd(children).title == "OTP"
  end

  defp make_elixir_otp_tree() do
    {:ok, elixir} =
      %Category{title: "Elixir"}
        |> Repo.insert

    {:ok, otp} =
      %Category{title: "OTP", parent_id: elixir.id}
        |> Repo.insert

    elixir
  end
end
