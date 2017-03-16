defmodule FirestormWeb.Web.TagHelpers do
  @moduledoc """
  View helpers for generating particular tags with some internal logic
  """

  import FirestormWeb.Web.Router.Helpers
  import Phoenix.HTML.{Link, Tag}

  @doc """
  Generate an abbr to wrap an ISO time to be rendered nicely via JS in the frontend.
  """
  def time_abbr(time) do
    time_string =
      time
      |> Timex.format!("{ISO:Extended}")

    content_tag(:abbr, class: "time", title: time_string) do
      time_string
    end
  end
end
