defmodule FirestormWeb.Web.FetchView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Store.ReplenishResponse

  def render("index.json", %ReplenishResponse{categories: categories, users: users, threads: threads, posts: posts}) do
    %{
      categories: [],
      threads: [],
      posts: [],
      users: [],
    }
  end
end
