defmodule FirestormWeb.Web.ControllerHelpers.Slugs do
  @moduledoc """
  Controller helpers for things related to slugs
  """

  def get_finder(id_or_slug) do
    case Integer.parse(id_or_slug) do
      :error ->
        id_or_slug
      {id, _} -> id
    end
  end
end
