defmodule FirestormWeb.Markdown do
  @moduledoc """
  Render a string as markdown in the FirestormWeb style.
  Then sanitize the resulting HTML (eventually...FIXME).
  """

  def render(body) do
    body
    |> as_html!()
  end

  defp as_html!(body) do
    body
    |> Earmark.as_html!(earmark_options())
    |> HtmlSanitizeEx.markdown_html
  end

  def earmark_options() do
    %Earmark.Options{
      # Prefix the `code` tag language class, as in `language-elixir`, for
      # proper support from http://prismjs.com/
      code_class_prefix: "language-",
      renderer: FirestormWeb.Markdown.HtmlRenderer,
      gfm: true,
      breaks: true
    }
  end
end
