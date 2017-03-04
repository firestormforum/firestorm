defmodule FirestormData.Schema.CategoryTest do
  alias FirestormData.{Category, Repo, User}
  use ExUnit.Case
  @valid_attributes %{
    title: "Something"
  }

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)

    user =
      %User{username: "knewter"}
      |> Repo.insert!

    {:ok, %{user: user}}
  end

  test "it requires a title" do
    changeset =
      %Category{}
        |> Category.changeset(Map.delete(@valid_attributes, :title))

    refute changeset.valid?
    assert changeset.errors[:title] == {"can't be blank", [validation: :required]}
  end

  test "it generates a slug" do
    create_category("OTP")

    assert Repo.one(Category).slug == "otp"
  end

  test "adding and retrieving categories" do
    # NOTE: We will not be doing repo stuff in the modules but i'm in a hurry
    assert [] == Category.categories

    create_category("OTP")
    create_category("Phoenix")

    assert [%{title: "OTP"}, %{title: "Phoenix"}] = Category.categories
  end

  test "it can have many views by users", %{user: user} do
    {:ok, elixir} = create_category("Elixir")
    {:ok, _} = create_view(elixir, user)
    {:ok, _} = create_view(elixir, user)
    {:ok, _} = create_view(elixir, user)

    assert Category.view_count(elixir) == 3
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

    {:ok, _otp} =
      %Category{title: "OTP", parent_id: elixir.id}
        |> Repo.insert

    elixir
  end

  defp create_category(title) do
    %Category{}
    |> Category.changeset(%{title: title})
    |> Repo.insert
  end

  defp create_view(category, user) do
    category
    |> Ecto.build_assoc(:views, %{user_id: user.id})
    |> Repo.insert
  end
end
