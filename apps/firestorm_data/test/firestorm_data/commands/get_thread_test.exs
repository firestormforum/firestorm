defmodule FirestormData.Commands.GetThreadTest do
  use ExUnit.Case
  alias FirestormData.Commands.{CreateCategory, CreateThread, GetThread}
  alias FirestormData.{Repo, Thread, User, Category}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo, ownership_timeout: 3_000_000)
  end

  describe "getting a thread" do
    setup [:create_user, :create_category, :create_thread]

    test "works via id",
      %{
        category_id: category_id,
        thread_id: thread_id
      }  do

      options =
        %GetThread{
          finder: thread_id,
          category_finder: category_id
        }

      assert {:ok, %{id: ^thread_id}} = GetThread.run(options)
    end

    test "works via slug",
      %{
        category_id: category_id,
        thread_id: thread_id
      }  do

      thread = Repo.get(Thread, thread_id)
      options =
        %GetThread{
          finder: thread.slug,
          category_finder: category_id
        }

      assert {:ok, %{id: ^thread_id}} = GetThread.run(options)
    end

    test "works with category slug",
      %{
        category_id: category_id,
        thread_id: thread_id
      }  do

      thread = Repo.get(Thread, thread_id)
      category = Repo.get(Category, category_id)

      options =
        %GetThread{
          finder: thread.slug,
          category_finder: category.slug
        }

      assert {:ok, %{id: ^thread_id}} = GetThread.run(options)
    end

    test "populates its category's ancestors",
      %{
        category_id: category_id,
        thread_id: thread_id
      }  do
      options =
        %GetThread{
          finder: thread_id,
          category_finder: category_id
        }

        {:ok, thread} = GetThread.run(options)
        ancestors =
          thread.category.ancestors
          |> Enum.map(fn(c) -> c.slug end)

      assert ancestors == ["elixir"]
    end
  end

  def create_category(_) do
    elixir_changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "Elixir"})

    {:ok, elixir_id} = CreateCategory.run(elixir_changeset)

    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "OTP", parent_id: elixir_id})

    {:ok, id} = CreateCategory.run(changeset)
    {:ok, %{category_id: id}}
  end

  def create_thread(%{user_id: user_id, category_id: category_id}) do
    changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{
        title: "OTP",
        body: "body",
        category_id: category_id,
        user_id: user_id
      })

    {:ok, id} = CreateThread.run(changeset)
    {:ok, %{thread_id: id}}
  end

  def create_user(_) do
    changeset =
      %User{}
      |> User.changeset(%{username: "sonny", email: "sonny@example.com"})

    {:ok, user} = Repo.insert(changeset)
    {:ok, %{user_id: user.id}}
  end
end
