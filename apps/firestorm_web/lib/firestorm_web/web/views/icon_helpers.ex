defmodule FirestormWeb.Web.IconHelpers do
  @moduledoc """
  View helpers for generating icons
  """

  import Phoenix.HTML.Tag

  @doc """
  Generate a FontAwesome icon with the provided name
  """
  def fa_icon(name) do
    content_tag(:i, class: "fa fa-#{name}") do
      ""
    end
  end
end
