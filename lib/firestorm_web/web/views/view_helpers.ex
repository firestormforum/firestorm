defmodule FirestormWeb.Web.ViewHelpers do
  @moduledoc """
  View helpers for grab-bag functions we find useful.
  """
  alias FirestormWeb.Forums.User

  def image_path(path) do
    if Mix.env == :prod do
      "/images/#{path}"
    else
      "http://localhost:8080/static/images/#{path}"
    end
  end

  def avatar_url(%User{email: email, username: username} = user, size \\ 256) do
    if email do
      gravatar_url(email, username, size)
    else
      adorable_url(username, size)
    end
  end

  defp gravatar_url(email, username, size) do
    email
    |> Exgravatar.gravatar_url(d: adorable_url(username, size), s: size)
  end

  defp adorable_url(username, size) do
    "https://api.adorable.io/avatars/#{size}/#{username}@adorable.png"
  end
end
