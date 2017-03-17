defmodule FirestormWeb.Web.Api.V1.HomeView do
  use FirestormWeb.Web, :view
  alias FirestormData.Category
  import Ecto.Query

  def render("index.json", %{categories: root_categories}) do
    categories = whole_category_tree_for(root_categories)
    threads = threads_for(categories)
    posts = posts_for(threads)
    users = users_for(posts)

    %{
      categories: Enum.map(categories, &category_json/1),
      threads: Enum.map(threads, &thread_json/1),
      posts: Enum.map(posts, &post_json/1),
      # NOTE: Unique these things fool! -ja
      users: Enum.map(users, &user_json/1)
    }
  end

  defp whole_category_tree_for(root_categories) do
    root_categories
    |> Enum.flat_map(fn(cat) ->
      descendants =
        cat
        |> Category.descendants()
        |> preload([threads: [posts: [:user], category: []], parent: []])
        |> Repo.all

      [cat|descendants]
    end)
  end

  defp threads_for(categories) do
    categories
    |> Enum.flat_map(&(&1.threads))
  end

  defp posts_for(threads) do
    threads
    |> Enum.flat_map(&(&1.posts))
  end

  defp users_for(posts) do
    posts
    |> Enum.map(&(&1.user))
  end

  def category_json(category) do
    %{
      id: category.id,
      title: category.title,
      slug: category.slug,
      inserted_at: category.inserted_at,
      updated_at: category.updated_at,
      parent_id: category.parent_id,
      children_ids: Enum.map(category.children || [], &(&1.id)),
      thread_ids: Enum.map(category.threads || [], &(&1.id))
    }
  end

  def thread_json(thread) do
    {:ok, user} = Thread.user(thread)
    %{
      id: thread.id,
      title: thread.title,
      slug: thread.slug,
      post_ids: Enum.map(thread.posts, &(&1.id)),
      user_id: user.id
    }
  end

  def post_json(post) do
    %{
      id: post.id,
      body: post.body,
      user_id: post.user_id
    }
  end

  def user_json(user) do
    %{
      id: user.id,
      username: user.username,
      email: user.email,
      name: user.name,
    }
  end
end
