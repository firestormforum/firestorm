defmodule FirestormWeb.Web.Api.V1.FetchController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{
    GetCategory,
  }
  alias FirestormWeb.Web.Api.V1.HomeView
  # NOTE: Don't use the Repo at all! Move this logic into a command
  alias FirestormData.{
    Repo,
    User,
    Thread,
    Post,
  }

  # "create" a Fetch, which returns the entities requested
  def create(conn, fetch) do
    category_finders =
      fetch["categories"]
      |> get_list
      |> Enum.map(&get_finder/1)

    thread_ids =
      fetch["threads"]
      |> get_list

    post_ids =
      fetch["posts"]
      |> get_list

    user_ids =
      fetch["users"]
      |> get_list

    # NOTE: This needs a command that knows how to get multiples rather than
    # singles, but ignoring that for now
    categories =
      category_finders
      |> Enum.map(&get_category/1)

    users =
      user_ids
      |> Enum.map(&get_user/1)

    threads =
      thread_ids
      |> Enum.map(&get_thread/1)

    posts =
      post_ids
      |> Enum.map(&get_post/1)

    conn
    |> render("index.json", categories: categories, users: users, threads: threads, posts: posts)
  end

  def get_category(finder) do
    with {:ok, c} <- GetCategory.run(%GetCategory{finder: finder}),
     do: c
  end

  def get_user(user_id) do
    Repo.get(User, user_id)
  end

  def get_thread(thread_id) do
    Thread
    |> where(id: ^thread_id)
    |> preload(:posts)
    |> Repo.one
  end

  def get_post(post_id) do
    Repo.get(Post, post_id)
  end

  def get_list(nil), do: []
  def get_list(a) when is_list(a), do: a
end
