defmodule FirestormWeb.Web.ReactionController do
  use FirestormWeb.Web, :controller
  alias FirestormData.{
    Repo,
    Post
  }
  alias FirestormData.Commands.{
    CreateReaction,
    GetThread,
  }
  plug FirestormWeb.Plugs.RequireUser

  def action(conn, _) do
    thread_finder = get_finder(conn.params["thread_id"])
    category_finder = get_finder(conn.params["category_id"])

    case GetThread.run(%GetThread{finder: thread_finder, category_finder: category_finder}) do
      {:ok, thread} ->
        case Repo.get(Post, conn.params["post_id"]) do
          nil ->
            conn
            |> put_flash(:error, "No such thread")
            |> redirect(to: page_path(conn, :home))
          post ->
            apply(__MODULE__, action_name(conn),
              [conn, conn.params, thread.category, thread, post])
        end

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such thread")
        |> redirect(to: page_path(conn, :home))
    end
  end

  def create(conn, %{"create_reaction" => create_reaction_params}, category, thread, post) do
    create_reaction_params =
      create_reaction_params
      |> Map.put("post_id", post.id)
      |> Map.put("user_id", current_user(conn).id)

    changeset =
      %CreateReaction{}
      |> CreateReaction.changeset(create_reaction_params)

    case changeset.valid? do
      true ->
        case CreateReaction.run(changeset) do
          {:ok, _reaction_id} ->
            conn
            |> put_flash(:info, "Reaction created successfully")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), thread.id))

          {:error, changeset} ->
            conn
            |> put_flash(:error, "Error creating reaction")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), thread.id))
        end
      false ->
        conn
        |> put_flash(:error, "Error creating reaction")
        |> redirect(to: category_thread_path(conn, :show, category_finder(category), thread.id))
    end
  end
end
