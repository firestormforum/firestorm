defmodule FirestormWeb.Web.Api.V1.UserView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Forums.User

  def render("show.json", %User{id: id, username: username, name: name, inserted_at: inserted_at, updated_at: updated_at}) do
    %{
      id: id,
      name: name,
      username: username,
      inserted_at: inserted_at,
      updated_at: updated_at,
    }
  end
end
