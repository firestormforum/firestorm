defmodule FirestormWeb.Web.ThreadController do
  use FirestormWeb.Web, :controller

  alias FirestormWeb.Repo
  alias FirestormWeb.Forums
  alias FirestormWeb.Forums.Thread
  plug FirestormWeb.Web.Plugs.RequireUser when action in [:new, :create, :watch, :unwatch, :participating, :watching]

  def action(conn, _) do
    if(conn.params["category_id"]) do
      category = Forums.get_category!(conn.params["category_id"])
      args = [conn, conn.params, category]
      apply(__MODULE__, action_name(conn), args)
    else
      apply(__MODULE__, action_name(conn), [conn, conn.params])
    end
  end

  def index(conn, _params, category) do
    threads = Forums.list_threads(category)
    render(conn, "index.html", threads: threads, category: category)
  end

  def recent(conn, _params) do
    threads =
      conn
      |> current_user()
      |> Forums.home_threads()

    render(conn, "recent.html", threads: threads)
  end

  def watching(conn, _params) do
    threads = Forums.watched_threads(current_user(conn))
    render(conn, "watching.html", threads: threads)
  end

  def participating(conn, _params) do
    threads = Forums.participating_threads(current_user(conn))
    render(conn, "participating.html", threads: threads)
  end

  def new(conn, _params, category) do
    changeset =
      %Thread{category_id: category.id}
      |> Forums.change_thread
    render(conn, "new.html", changeset: changeset, category: category)
  end

  def create(conn, %{"thread" => thread_params}, category) do
    case Forums.create_thread(category, current_user(conn), %{title: thread_params["title"], body: thread_params["body"]}) do
      {:ok, thread} ->
        conn
        |> put_flash(:info, "Thread created successfully.")
        |> redirect(to: category_thread_path(conn, :show, category, thread))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, category: category)
    end
  end

  def show(conn, %{"id" => id}, category) do
    thread =
      category
      |> Forums.get_thread!(id)
      |> Repo.preload(posts: [:user])

    all_posts =
      thread.posts
      |> Enum.map(&Forums.decorate_post_oembeds/1)

    [first_post | posts] = all_posts

    watched =
      if current_user(conn) do
        thread |> Forums.watched_by?(current_user(conn))
      else
        false
      end

    # Record a view for each post in this thread
    if current_user(conn) do
      for post <- thread.posts do
        conn
        |> current_user()
        |> Forums.view(post)
      end
    end

    render(conn, "show.html", thread: thread, category: category, first_post: first_post, posts: posts, watched: watched)
  end

  def watch(conn, %{"id" => id}, category) do
    thread =
      Forums.get_thread!(category, id)

    conn
    |> current_user()
    |> Forums.watch(thread)

    conn
    |> redirect(to: category_thread_path(conn, :show, category.id, id))
  end

  def unwatch(conn, %{"id" => id}, category) do
    thread =
      Forums.get_thread!(category, id)

    conn
    |> current_user()
    |> Forums.unwatch(thread)

    conn
    |> redirect(to: category_thread_path(conn, :show, category.id, id))
  end

  def edit(conn, %{"id" => id}, category) do
    thread = Forums.get_thread!(category, id)
    changeset = Forums.change_thread(thread)
    render(conn, "edit.html", thread: thread, changeset: changeset, category: category)
  end

  def update(conn, %{"id" => id, "thread" => thread_params}, category) do
    thread = Forums.get_thread!(category, id)

    case Forums.update_thread(thread, thread_params) do
      {:ok, thread} ->
        conn
        |> put_flash(:info, "Thread updated successfully.")
        |> redirect(to: category_thread_path(conn, :show, category, thread))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", thread: thread, changeset: changeset, category: category)
    end
  end

  def delete(conn, %{"id" => id}, category) do
    thread = Forums.get_thread!(category, id)
    {:ok, _thread} = Forums.delete_thread(thread)

    conn
    |> put_flash(:info, "Thread deleted successfully.")
    |> redirect(to: category_thread_path(conn, :index, category))
  end
end
