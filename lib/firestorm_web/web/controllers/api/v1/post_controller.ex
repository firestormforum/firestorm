defmodule FirestormWeb.Web.Api.V1.PostController do
  use FirestormWeb.Web, :controller
  alias FirestormWeb.Forums

  def create(conn, %{"post" => post_params, "thread_id" => thread_id}) do
    with thread when not is_nil(thread) <- Forums.get_thread(thread_id),
         user when not is_nil(user) <- conn.assigns[:current_user],
         {:ok, post} <- Forums.create_post(thread, user, %{body: post_params["body"]}) do
      conn
      |> put_status(201)
      |> render("show.json", post)
    else
      x ->
        IO.inspect x

        conn
        |> put_status(:unprocessable_entity)
        # FIXME: Send back errors that are meaningful
        |> json(%{"error" => "Failed to create post"})
    end
  end
end
