defmodule FirestormWeb.Web.Api.V1.ThreadView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Forums.Thread

  def render("show.json", %Thread{id: id, title: title, inserted_at: inserted_at, updated_at: updated_at, category_id: category_id}) do
    %{
      id: id,
      title: title,
      inserted_at: inserted_at,
      updated_at: updated_at,
      category_id: category_id
    }
  end
end
