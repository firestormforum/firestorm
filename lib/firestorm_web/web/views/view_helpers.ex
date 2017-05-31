defmodule FirestormWeb.Web.ViewHelpers do
  @moduledoc """
  View helpers for grab-bag functions we find useful.
  """

  def image_path(path) do
    if Mix.env == :prod do
      "/images/#{path}"
    else
      "http://localhost:8080/static/images/#{path}"
    end
  end
end
