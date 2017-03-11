defmodule FirestormWeb.Web.ThreadView do
  use FirestormWeb.Web, :view
  alias FirestormData.Thread
  import FirestormWeb.Web.SlugHelpers

  def user(thread) do
    {:ok, user} = Thread.user(thread)
    user
  end

  def markdown(body) do
    body
    |> as_html!
    |> raw
  end

  def back("show.html", conn) do
    category = conn.assigns[:category]
    category_path(conn, :show, category_finder(category))
  end
  def back(_template, _conn), do: nil

  # Options to control how earmark works. We should probably expose these in
  # some fashion in the configuration, so people can more easily tweak the
  # forum's behaviour to their liking without changing code. Not a high priority
  # though.
  defp earmark_options() do
    %Earmark.Options{
      # Add some "github-flavored-markdown" features (tables, strikethrough, &c.)
      gfm: true,
      # Honor line breaks in markdown (i.e. not quite to spec) - I think that
      # this is useful behaviour for a forum but I'm not above being convinced
      # I'm wrong and dumb on this
      breaks: true,
      # Prefix the `code` tag language class, as in `language-elixir`, for
      # proper support from http://prismjs.com/
      code_class_prefix: "language-",
      # Use our custom Earmark renderer so we can take advantage of various
      # features in Prism that require us to tweak the generated `pre/code` tags
      renderer: FirestormWeb.Earmark.HtmlRenderer,
    }
  end

  defp as_html!(body) do
    body
    |> Earmark.as_html!(earmark_options())
  end
end
