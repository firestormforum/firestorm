defmodule FirestormWeb.Web.StoreChannel do
  use FirestormWeb.Web, :channel
  # use Appsignal.Instrumentation.Decorators
  alias FirestormWeb.Store.{ReplenishResponse, ReplenishRequest}
  alias FirestormWeb.Web.Api.V1.FetchView
  alias FirestormWeb.{Repo, Forums}

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

  # @decorate channel_action()
  def handle_in("fetch_home_data", _, socket) do
    # TODO: Make this not awful
    categories = Forums.list_categories()

    # TODO: Set the current user
    threads =
      categories
      |> Enum.flat_map(&Forums.recent_threads(&1, nil))

    posts =
      threads
      |> Enum.map(&Repo.preload(&1, posts: [:user]))
      |> Enum.flat_map(& &1.posts)
      |> Enum.map(&Forums.decorate_post_oembeds/1)

    users =
      posts
      |> Enum.map(& &1.user)
      |> Enum.uniq_by(& &1.id)

    {:reply,
     {:ok,
      FetchView.render("index.json", %ReplenishResponse{
        categories: categories,
        threads: threads,
        posts: posts,
        users: users
      })}, socket}
  end

  # @decorate channel_action()
  def handle_in("fetch", replenish_request, socket) do
    replenish_request = Poison.decode!(Poison.encode!(replenish_request), as: %ReplenishRequest{})

    # TODO: Make this not awful
    categories =
      replenish_request.categories
      |> Enum.map(&Forums.get_category!(&1))

    threads =
      replenish_request.threads
      |> Enum.map(&Forums.get_thread!(&1))

    posts =
      replenish_request.posts
      |> Enum.map(&Forums.get_post!(&1))

    users =
      replenish_request.users
      |> Enum.map(&Forums.get_user!(&1))

    {:reply,
     {:ok,
      FetchView.render("index.json", %ReplenishResponse{
        categories: categories,
        threads: threads,
        posts: posts,
        users: users
      })}, socket}
  end
end
