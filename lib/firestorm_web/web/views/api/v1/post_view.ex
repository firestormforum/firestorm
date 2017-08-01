defmodule FirestormWeb.Web.Api.V1.PostView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Forums.Post

  def render("show.json", %Post{id: id, body: body, inserted_at: inserted_at, updated_at: updated_at, thread_id: thread_id}) do
    %{
      id: id,
      body: body,
      inserted_at: inserted_at,
      updated_at: updated_at,
      thread_id: thread_id
    }
  end
end
