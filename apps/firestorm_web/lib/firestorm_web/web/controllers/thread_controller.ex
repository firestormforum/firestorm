defmodule FirestormWeb.Web.ThreadController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{
    GetCategory,
    CreateThread,
    GetThread,
    ViewPost,
    FollowThread,
    UnfollowThread,
    TagThread,
  }
  alias FirestormData.Followable

  def action(conn, _) do
    finder = get_finder(conn.params["category_id"])
    case GetCategory.run(%GetCategory{finder: finder}) do
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

    tag_thread_changeset =
      %TagThread{}
      |> TagThread.changeset(%{})

    case GetThread.run(%GetThread{finder: finder, category_id: category.id}) do
      {:ok, thread} ->
        [first_post | posts] = thread.posts
        # FIXME: lol don't do this
        view_posts(thread.posts, current_user(conn))
        category_breadcrumbs =
          [thread.category | Repo.all Category.ancestors(thread.category)]
          |> Enum.reverse

        following = case current_user(conn) do
          nil -> false
          u -> Followable.followed_by?(thread, u)
        end

        conn
        |> render(
             "show.html",
             thread: thread,
             category: category,
             first_post: first_post,
             posts: posts,
             category_breadcrumbs: category_breadcrumbs,
             following: following,
             tag_thread_changeset: tag_thread_changeset
           )

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such thread")
        |> redirect(to: category_path(conn, :show, category_finder(category)))
    end
  end

  def tag(conn, %{"thread_id" => id_or_slug, "tag_thread" => tag_thread_params}, category) do
    finder = get_finder(id_or_slug)

    case GetThread.run(%GetThread{finder: finder, category_id: category.id}) do
      {:ok, thread} ->
        changeset =
          %TagThread{}
          |> TagThread.changeset(
            tag_thread_params
            |> Map.put("thread_id", thread.id)
          )

        case TagThread.run(changeset) do
          {:ok, _} ->
            conn
            |> put_flash(:info, "Tagged thread")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), thread.id))

          {:error, e} ->
            conn
            |> put_flash(:error, "An error occurred #{inspect e}")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), thread.id))
        end

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such thread")
        |> redirect(to: category_path(conn, :show, category_finder(category)))
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
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), thread_id))

          {:error, changeset} ->
            conn
            |> render("new.html", changeset: changeset, category: category)
        end

      false ->
        conn
        |> render("new.html", changeset: changeset, category: category)
    end
  end

  def follow(conn, %{"thread_id" => id_or_slug}, category) do
    finder = get_finder(id_or_slug)

    case GetThread.run(%GetThread{finder: finder, category_id: category.id}) do
      {:ok, thread} ->
        changeset =
          %FollowThread{}
          |> FollowThread.changeset(%{user_id: current_user(conn).id, thread_id: thread.id})

        case FollowThread.run(changeset) do
          {:ok, _} ->
            conn
            |> put_flash(:info, "You are now following this thread")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), id_or_slug))

          {:error, _} ->
            conn
            |> put_flash(:error, "An error occurred")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), id_or_slug))
        end

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such thread")
        |> redirect(to: category_thread_path(conn, :show, category_finder(category), id_or_slug))
    end
  end

  def unfollow(conn, %{"thread_id" => id_or_slug}, category) do
    finder = get_finder(id_or_slug)

    case GetThread.run(%GetThread{finder: finder, category_id: category.id}) do
      {:ok, thread} ->
        changeset =
          %UnfollowThread{}
          |> UnfollowThread.changeset(%{user_id: current_user(conn).id, thread_id: thread.id})

        case UnfollowThread.run(changeset) do
          :ok ->
            conn
            |> put_flash(:info, "You are no longer following this thread")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), id_or_slug))
        end

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such thread")
        |> redirect(to: category_thread_path(conn, :show, category_finder(category), id_or_slug))
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
