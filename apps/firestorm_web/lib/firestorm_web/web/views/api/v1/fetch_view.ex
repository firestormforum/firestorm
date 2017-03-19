defmodule FirestormWeb.Web.Api.V1.FetchView do
  use FirestormWeb.Web, :view
  alias FirestormData.Category
  import Ecto.Query

  def render("index.json", %{categories: categories, users: users, threads: threads, posts: posts}) do
    %{
      categories: Enum.map(categories, &category_json/1),
      threads: Enum.map(threads, &thread_json/1),
      posts: Enum.map(posts, &post_json/1),
      users: Enum.map(users, &user_json/1)
    }
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
      category_id: thread.category_id,
      post_ids: Enum.map(thread.posts, &(&1.id)),
      user_id: user.id
    }
  end

  def post_json(post) do
    %{
      id: post.id,
      body: post.body,
      user_id: post.user_id,
      thread_id: post.thread_id,
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
