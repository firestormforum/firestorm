defmodule FirestormWeb.ControllerHelpers.Slugs do
  def get_finder(id_or_slug) do
    case Integer.parse(id_or_slug) do
      :error ->
        id_or_slug
      {id, _} -> id
    end
  end
end
