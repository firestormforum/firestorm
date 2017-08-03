defmodule FirestormWeb.Web.Api.V1.CategoryView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Forums.Category

  def render("show.json", %Category{id: id, title: title, slug: slug, inserted_at: inserted_at, updated_at: updated_at}) do
    %{
      id: id,
      title: title,
      slug: slug,
      inserted_at: inserted_at,
      updated_at: updated_at
    }
  end
end
