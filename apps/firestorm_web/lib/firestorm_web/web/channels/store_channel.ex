defmodule FirestormWeb.Web.StoreChannel do
  use FirestormWeb.Web, :channel
  alias FirestormData.Commands.{
    GetHomeCategories,
    GetCategory
  }
  alias FirestormWeb.Web.Api.V1.{
    HomeView,
    FetchView
  }
  alias FirestormData.{
    Repo,
    User,
    Thread,
    Post,
  }
  import Ecto.Query
  import FirestormWeb.Web.ControllerHelpers.Slugs

  def join("store:home", payload, socket) do
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  def join("store:fetch", payload, socket) do
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end

  def handle_in("fetch_home_data", %{}, socket) do
    {:ok, categories} =
      %GetHomeCategories{user_id: 1}
      |> GetHomeCategories.run

    response =
      "index.json"
      |> HomeView.render(%{categories: categories})

    {:reply, {:ok, response}, socket}
  end
  def handle_in("fetch", fetch, socket) do
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

    response =
      "index.json"
      |> FetchView.render(%{
           categories: categories,
           users: users,
           threads: threads,
           posts: posts
         })

    {:reply, {:ok, response}, socket}
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
