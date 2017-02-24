defmodule Phoenix.HTML.Format do
  @moduledoc """
  Helpers related to formatting text.
  """

  @doc ~S"""
  Returns text transformed into HTML using simple formatting rules.

  Two or more consecutive newlines `\n\n` are considered as a paragraph
  and text between them is wrapped in `<p>` tags.
  One newline `\n` is considered as a linebreak and a `<br>` tag is inserted.

  ## Examples

      iex> text_to_html("Hello\n\nWorld") |> safe_to_string
      "<p>Hello</p>\n<p>World</p>\n"

      iex> text_to_html("Hello\nWorld") |> safe_to_string
      "<p>Hello<br>\nWorld</p>\n"

      iex> opts = [wrapper_tag: :div, attributes: [class: "p"]]
      ...> text_to_html("Hello\n\nWorld", opts) |> safe_to_string
      "<div class=\"p\">Hello</div>\n<div class=\"p\">World</div>\n"

  ## Options

    * `:escape` - if `false` does not html escape input (default: `true`)
    * `:wrapper_tag` - tag to wrap each paragraph (default: `:p`)
    * `:attributes` - html attributes of the wrapper tag (default: `[]`)
    * `:insert_brs` - if `true` insert `<br>` for single line breaks (default: `true`)

  """
  @spec text_to_html(Phoenix.HTML.unsafe, Keyword.t) :: Phoenix.HTML.safe
  def text_to_html(string, opts \\ []) do
    escape?     = Keyword.get(opts, :escape, true)
    wrapper_tag = Keyword.get(opts, :wrapper_tag, :p)
    attributes  = Keyword.get(opts, :attributes, [])
    insert_brs? = Keyword.get(opts, :insert_brs, true)

    string
    |> maybe_html_escape(escape?)
    |> String.split("\n\n", trim: true)
    |> Enum.filter_map(&not_blank?/1, &wrap_paragraph(&1, wrapper_tag, attributes, insert_brs?))
    |> Phoenix.HTML.html_escape
  end

  defp maybe_html_escape(string, true),  do: Plug.HTML.html_escape(string)
  defp maybe_html_escape(string, false), do: string

  defp not_blank?(" " <> rest),  do: not_blank?(rest)
  defp not_blank?("\n" <> rest), do: not_blank?(rest)
  defp not_blank?(""),           do: false
  defp not_blank?(_),            do: true

  defp wrap_paragraph(text, tag, attributes, insert_brs?) do
    [Phoenix.HTML.Tag.content_tag(tag, insert_brs(text, insert_brs?), attributes), ?\n]
  end

  defp insert_brs(text, false) do
    text
    |> String.split("\n", trim: true)
    |> Enum.intersperse(?\s)
    |> Phoenix.HTML.raw
  end

  defp insert_brs(text, true) do
    text
    |> String.split("\n", trim: true)
    |> Enum.map(&Phoenix.HTML.raw/1)
    |> Enum.intersperse([Phoenix.HTML.Tag.tag(:br), ?\n])
  end
end
