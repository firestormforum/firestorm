defmodule FirestormWeb.Web.ThreadController do
  use FirestormWeb.Web, :controller

  alias FirestormWeb.Repo
  alias FirestormWeb.Forums
  alias FirestormWeb.Forums.Thread
  plug FirestormWeb.Web.Plugs.RequireUser when action in [:new, :create, :watch, :unwatch]

  def action(conn, _) do
    category = Forums.get_category!(conn.params["category_id"])
    args = [conn, conn.params, category]
    apply(__MODULE__, action_name(conn), args)
  end

  def index(conn, _params, category) do
    threads = Forums.list_threads(category)
    render(conn, "index.html", threads: threads, category: category)
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
      Forums.get_thread!(category, id)
      |> Repo.preload(posts: [:user])

    [ first_post | posts ] = thread.posts

    watched =
      if current_user(conn) do
        thread |> Forums.watched_by?(current_user(conn))
      else
        false
      end

    render(conn, "show.html", thread: thread, category: category, first_post: first_post, posts: posts, watched: watched)
  end

  def watch(conn, %{"id" => id}, category) do
    thread =
      Forums.get_thread!(category, id)

    current_user(conn)
    |> Forums.watch(thread)

    conn
    |> redirect(to: category_thread_path(conn, :show, category.id, id))
  end

  def unwatch(conn, %{"id" => id}, category) do
    thread =
      Forums.get_thread!(category, id)

    current_user(conn)
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
