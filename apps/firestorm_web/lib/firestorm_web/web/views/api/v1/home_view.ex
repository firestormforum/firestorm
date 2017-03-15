defmodule FirestormWeb.Web.Api.V1.HomeView do
  use FirestormWeb.Web, :view

  def render("index.json", %{categories: categories}) do
    %{
      categories: Enum.map(categories, &category_json/1)
    }
  end

  def category_json(category) do
    %{
      id: category.id,
      title: category.title,
      inserted_at: category.inserted_at,
      updated_at: category.updated_at,
      threads: Enum.map(category.threads, &thread_json/1)
    }
  end

  def thread_json(thread) do
    {:ok, user} = Thread.user(thread)
    %{
      id: thread.id,
      title: thread.title,
      posts: Enum.map(thread.posts, &post_json/1),
      user: user_json(user)
    }
  end

  def post_json(post) do
    %{
      id: post.id,
      body: post.body,
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
