defmodule FirestormWeb.Web.Api.V1.FetchView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Store.ReplenishResponse
  alias FirestormWeb.Web.Api.V1.{CategoryView, ThreadView, PostView, UserView}

  def render("index.json", %ReplenishResponse{
        categories: categories,
        users: users,
        threads: threads,
        posts: posts
      }) do
    %{
      categories: render_categories(categories),
      threads: render_threads(threads),
      posts: render_posts(posts),
      users: render_users(users)
    }
  end

  defp render_categories(categories) do
    categories
    |> Enum.map(&CategoryView.render("show.json", &1))
  end

  defp render_threads(threads) do
    threads
    |> Enum.map(&ThreadView.render("show.json", &1))
  end

  defp render_posts(posts) do
    posts
    |> Enum.map(&PostView.render("show.json", &1))
  end

  defp render_users(users) do
    users
    |> Enum.map(&UserView.render("show.json", &1))
  end
end
