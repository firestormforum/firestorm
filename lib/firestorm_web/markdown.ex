defmodule FirestormWeb.Markdown do
  @moduledoc """
  Sanitize a string's HTML, then render the string as markdown in the FirestormWeb style.
  """

  def render(body) do
    body
    |> as_html!()
  end

  defp as_html!(body) do
    body
    |> HtmlSanitizeEx.markdown_html()
    |> Earmark.as_html!(earmark_options())
  end

  def earmark_options() do
    %Earmark.Options{
      # Prefix the `code` tag language class, as in `language-elixir`, for
      # proper support from http://prismjs.com/
      code_class_prefix: "language-",
      renderer: FirestormWeb.Markdown.HtmlRenderer,
      gfm: true,
      breaks: true,
      smartypants: false
    }
  end
end
