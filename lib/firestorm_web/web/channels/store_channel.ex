defmodule FirestormWeb.Web.StoreChannel do
  use FirestormWeb.Web, :channel
  alias FirestormWeb.Store.{ReplenishResponse, ReplenishRequest}
  alias FirestormWeb.Web.Api.V1.FetchView
  alias FirestormWeb.Forums

  def join("store:fetch", payload, socket) do
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  defp authorized?(_) do
    true
  end

  def handle_in("fetch", replenish_request, socket) do
    replenish_request = Poison.decode!(Poison.encode!(replenish_request), as: %ReplenishRequest{})

    # TODO: Make this not awful
    categories =
      replenish_request.categories
      |> Enum.map(&(Forums.get_category!(&1)))

    threads =
      replenish_request.threads
      |> Enum.map(&(Forums.get_thread!(&1)))

    posts =
      replenish_request.posts
      |> Enum.map(&(Forums.get_post!(&1)))

    users =
      replenish_request.users
      |> Enum.map(&(Forums.get_user!(&1)))


    {:reply, {:ok, FetchView.render("index.json", %ReplenishResponse{categories: categories, threads: threads, posts: posts, users: users})}, socket}
  end
end
