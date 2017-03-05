defmodule FirestormWeb.Web.ThreadController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{
    GetCategory,
    CreateThread,
    GetThread,
    ViewPost
  }

  def action(conn, _) do
    case GetCategory.run(%GetCategory{finder: conn.params["category_id"]}) do
      {:ok, category} ->
        apply(__MODULE__, action_name(conn),
          [conn, conn.params, category])

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such category")
        |> redirect(to: page_path(conn, :home))
    end
  end

  def show(conn, %{"id" => id_or_slug}, category) do
    finder = get_finder(id_or_slug)

    case GetThread.run(%GetThread{finder: finder, category_finder: category.id}) do
      {:ok, thread} ->
        [first_post | posts] = thread.posts
        # FIXME: lol don't do this
        view_posts(thread.posts, current_user(conn))
        category_breadcrumbs =
          [thread.category | thread.category.ancestors ]
          # Repo.all Category.ancestors(thread.category)]
          |> Enum.reverse

        conn
        |> render(
             "show.html",
             thread: thread,
             category: category,
             first_post: first_post,
             posts: posts,
             category_breadcrumbs: category_breadcrumbs
           )

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such thread")
        |> redirect(to: category_path(conn, :show, category.slug))
    end
  end

  def new(conn, _, category) do
    changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{})

    conn
    |> render("new.html", changeset: changeset, category: category)
  end

  def create(conn, %{"create_thread" => create_thread_params}, category) do
    create_thread_params =
      create_thread_params
      |> Map.put("category_id", category.id)
      |> Map.put("user_id", current_user(conn).id)

    changeset =
      %CreateThread{}
      |> CreateThread.changeset(create_thread_params)

    case changeset.valid? do
      true ->
        case CreateThread.run(changeset) do
          {:ok, thread_id} ->
            conn
            |> put_flash(:info, "Thread created successfully")
            |> redirect(to: category_thread_path(conn, :show, category.slug, thread_id))

          {:error, changeset} ->
            conn
            |> render("new.html", changeset: changeset, category: category)
        end

      false ->
        conn
        |> render("new.html", changeset: changeset, category: category)
    end
  end

  defp view_posts(_posts, nil), do: :ok
  defp view_posts(posts, user) do
    for post <- posts do
      %ViewPost{}
      |> ViewPost.changeset(%{user_id: user.id, post_id: post.id})
      |> ViewPost.run
    end
  end
end
