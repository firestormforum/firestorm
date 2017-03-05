defmodule FirestormWeb.Web.ViewHelpers do
  @moduledoc """
  View helpers for grab-bag functions we find useful.
  """

  import Phoenix.HTML.Tag

  def image_path(path) do
    if Mix.env == :prod do
      "/images/#{path}"
    else
      "http://localhost:8080/assets/images/#{path}"
    end
  end

  def icon(name) do
    tag(:i, class: "fa fa-#{name}")
  end
end
