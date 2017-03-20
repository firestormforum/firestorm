defmodule FirestormWeb.Web.Api.V1.FetchView do
  use FirestormWeb.Web, :view
  alias FirestormData.Category
  import Ecto.Query
  alias FirestormWeb.Web.Api.V1.HomeView

  def render("index.json", %{categories: categories, users: users, threads: threads, posts: posts}) do
    %{
      categories: Enum.map(categories, &HomeView.category_json/1),
      threads: Enum.map(threads, &HomeView.thread_json/1),
      posts: Enum.map(posts, &HomeView.post_json/1),
      users: Enum.map(users, &HomeView.user_json/1)
    }
  end
end
