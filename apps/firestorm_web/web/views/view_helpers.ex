defmodule FirestormWeb.ViewHelpers do
  def image_path(path) do
    if Mix.env == :prod do
      "/images/#{path}"
    else
      "http://localhost:8080/assets/images/#{path}"
    end
  end
end
