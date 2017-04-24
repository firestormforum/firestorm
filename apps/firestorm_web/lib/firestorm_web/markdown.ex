defmodule FirestormWeb.Markdown do

  @moduledoc """
  Render a string as markdown in the FirestormWeb style.
  Then sanitize the resulting HTML.
  """

  def render(body) do
    body
    |> as_html!
  end

  defp as_html!(body) do
    body
    |> Earmark.as_html!(earmark_options())
    |> HtmlSanitizeEx.markdown_html
  end

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
end
