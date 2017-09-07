# This is a clone of https://raw.githubusercontent.com/pragdave/earmark/894c3a70f47d586665a772b0151cb3b73a3e89bf/lib/earmark/html_renderer.ex
#
# Justification for forking this module:
# - Earmark explicitly provides support for this, but we can't reuse the
#   existing render_* functions as they are private.
# - No existing ability to customize the classes that are produced for the `pre`
#   tag, which is necessary to use http://prismjs.com/plugins/line-numbers/
defmodule FirestormWeb.Markdown.HtmlRenderer do
  # credo:disable-for-this-file

  alias  Earmark.Block
  alias  Earmark.Context
  alias  Earmark.Options
  import Earmark.Inline,  only: [ convert: 3 ]
  import Earmark.Helpers, only: [ escape: 2 ]
  import Earmark.Helpers.HtmlHelpers

  def render(blocks, context=%Context{options: %Options{mapper: mapper}}) do
    html =
      mapper.(blocks, &(render_block(&1, context)))
    IO.iodata_to_binary(html)
  end

  #############
  # Paragraph #
  #############
  defp render_block(%Block.Para{lnb: lnb, lines: lines, attrs: attrs}, context) do
    alias FirestormWeb.Markdown.{AutoLinker, EmojiReplacer}

    lines =
      lines
      |> convert(lnb, context)
      |> AutoLinker.run()
      |> EmojiReplacer.run()

    add_attrs!("<p>#{lines}</p>\n", attrs, [], lnb)
  end

  ########
  # Html #
  ########
  defp render_block(%Block.Html{html: html}, _context) do
    Enum.intersperse(html, ?\n)
  end

  defp render_block(%Block.HtmlOther{html: html}, _context) do
    Enum.intersperse(html, ?\n)
  end

  #########
  # Ruler #
  #########
  defp render_block(%Block.Ruler{lnb: lnb, type: "-", attrs: attrs}, _context) do
    add_attrs!("<hr/>\n", attrs, [{"class", ["thin"]}], lnb)
  end

  defp render_block(%Block.Ruler{lnb: lnb, type: "_", attrs: attrs}, _context) do
    add_attrs!("<hr/>\n", attrs, [{"class", ["medium"]}], lnb)
  end

  defp render_block(%Block.Ruler{lnb: lnb, type: "*", attrs: attrs}, _context) do
    add_attrs!("<hr/>\n", attrs, [{"class", ["thick"]}], lnb)
  end

  ###########
  # Heading #
  ###########
  defp render_block(%Block.Heading{lnb: lnb, level: level, content: content, attrs: attrs}, context) do
    converted = convert(content, lnb, context)
    html = "<h#{level}>#{converted}</h#{level}>\n"
    add_attrs!(html, attrs, [], lnb)
  end

  ##############
  # Blockquote #
  ##############

  defp render_block(%Block.BlockQuote{lnb: lnb, blocks: blocks, attrs: attrs}, context) do
    body = render(blocks, context)
    html = "<blockquote>#{body}</blockquote>\n"
    add_attrs!(html, attrs, [], lnb)
  end

  #########
  # Table #
  #########

  defp render_block(%Block.Table{lnb: lnb, header: header, rows: rows, alignments: aligns, attrs: attrs}, context) do
    cols = for _align <- aligns, do: "<col>\n"
    html = [ add_attrs!("<table>\n", attrs, [], lnb), "<colgroup>\n", cols, "</colgroup>\n" ]

    html = if header do
      [ html, "<thead>\n",
        add_table_rows(context, [header], "th", aligns, lnb),
        "</thead>\n" ]
    else
      html
    end

    [ html, add_table_rows(context, rows, "td", aligns, lnb), "</table>\n" ]
  end

  ########
  # Code #
  ########

  defp pre_classes() do
    "line-numbers"
  end

  defp render_block(%Block.Code{lnb: lnb, language: language, attrs: attrs} = block, %Context{options: options}) do
    class = if language, do: ~s{ class="#{code_classes( language, options.code_class_prefix)}"}, else: ""
    tag = ~s[<pre class="#{pre_classes()}"><code#{class}>]
    lines = options.render_code.(block)
    html = ~s[#{tag}#{lines}</code></pre>\n]
    add_attrs!(html, attrs, [], lnb)
  end

  #########
  # Lists #
  #########

  defp render_block(%Block.List{lnb: lnb, type: type, blocks: items, attrs: attrs, start: start}, context) do
    content = render(items, context)
    html = "<#{type}#{start}>\n#{content}</#{type}>\n"
    add_attrs!(html, attrs, [], lnb)
  end

  # format a single paragraph list item, and remove the para tags
  defp render_block(%Block.ListItem{lnb: lnb, blocks: blocks, spaced: false, attrs: attrs}, context)
  when length(blocks) == 1 do
    content = render(blocks, context)
    content = Regex.replace(~r{</?p>}, content, "")
    html = "<li>#{content}</li>\n"
    add_attrs!(html, attrs, [], lnb)
  end

  # format a spaced list item
  defp render_block(%Block.ListItem{lnb: lnb, blocks: blocks, attrs: attrs}, context) do
    content = render(blocks, context)
    html = "<li>#{content}</li>\n"
    add_attrs!(html, attrs, [], lnb)
  end

  ##################
  # Footnote Block #
  ##################

  defp render_block(%Block.FnList{blocks: footnotes}, context) do
    items = Enum.map(footnotes, fn(note) ->
      blocks = append_footnote_link(note)
      %Block.ListItem{attrs: "#fn:#{note.number}", type: :ol, blocks: blocks}
    end)
    html = render_block(%Block.List{type: :ol, blocks: items}, context)
    Enum.join([~s[<div class="footnotes">], "<hr>", html, "</div>"], "\n")
  end

  #######################################
  # Isolated IALs are rendered as paras #
  #######################################

  defp render_block(%Block.Ial{verbatim: verbatim}, _context) do
    "<p>{:#{verbatim}}</p>\n"
  end

  ####################
  # IDDef is ignored #
  ####################

  defp render_block(%Block.IdDef{}, _context), do: ""

  ###########
  # Plugins #
  ###########

  defp render_block(%Block.Plugin{lines: lines, handler: handler}, _context) do
    case handler.as_html(lines) do
      html when is_list(html) -> html
      {html, errors}          -> emit_messages(html, errors)
      html                    -> [html]
    end
  end

  #####################################
  # And here are the inline renderers #
  #####################################

  def br,                  do: "<br/>"
  def codespan(text),      do: ~s[<code class="inline">#{text}</code>]
  def em(text),            do: "<em>#{text}</em>"
  def strong(text),        do: "<strong>#{text}</strong>"
  def strikethrough(text), do: "<del>#{text}</del>"

  def link(url, text),        do: ~s[<a href="#{url}">#{text}</a>]
  def link(url, text, nil),   do: ~s[<a href="#{url}">#{text}</a>]
  def link(url, text, title), do: ~s[<a href="#{url}" title="#{title}">#{text}</a>]

  def image(path, alt, nil) do
    ~s[<img src="#{path}" alt="#{alt}"/>]
  end

  def image(path, alt, title) do
    ~s[<img src="#{path}" alt="#{alt}" title="#{title}"/>]
  end

  def footnote_link(ref, backref, number), do: ~s[<a href="##{ref}" id="#{backref}" class="footnote" title="see footnote">#{number}</a>]

  # Table rows
  defp add_table_rows(context, rows, tag, aligns, lnb) do
    numbered_rows = rows
      |> Enum.zip(Stream.iterate(lnb, &(&1 + 1)))
    for {row, lnb1} <- numbered_rows, do: "<tr>\n#{add_tds(context, row, tag, aligns, lnb1)}\n</tr>\n"
  end

  defp add_tds(context, row, tag, aligns, lnb) do
    (1..length(row))
    Enum.reduce({[], row}, add_td_fn(context, row, tag, aligns, lnb))
    |> elem(0)
    |> Enum.reverse
  end

  defp add_td_fn(context, row, tag, aligns, lnb) do
    fn n, {acc, _row} ->
      style = cond do
        align = Enum.at(aligns, n - 1) ->
          " style=\"text-align: #{align}\""
        true ->
          ""
      end
      col = Enum.at(row, n - 1)
      converted = convert(col, lnb,  context)
      {["<#{tag}#{style}>#{converted}</#{tag}>" | acc], row}
    end
  end


  ###############################
  # Append Footnote Return Link #
  ###############################

  def append_footnote_link(note=%Block.FnDef{}) do
    fnlink = ~s[<a href="#fnref:#{note.number}" title="return to article" class="reversefootnote">&#x21A9;</a>]
    [ last_block | blocks ] = Enum.reverse(note.blocks)
    last_block = append_footnote_link(last_block, fnlink)
    Enum.reverse([last_block | blocks])
    |> List.flatten
  end

  def append_footnote_link(block=%Block.Para{lines: lines}, fnlink) do
    [ last_line | lines ] = Enum.reverse(lines)
    last_line = "#{last_line}&nbsp;#{fnlink}"
    [put_in(block.lines, Enum.reverse([last_line | lines]))]
  end

  def append_footnote_link(block, fnlink) do
    [block, %Block.Para{lines: fnlink}]
  end

  def render_code(%Block.Code{lines: lines}) do
    lines |> Enum.join("\n") |> escape(true)
  end

  defp code_classes(language, prefix) do
   ["" | String.split( prefix || "" )]
     |> Enum.map( fn pfx -> "#{pfx}#{language}" end )
     |> Enum.join(" ")
  end


  #################
  # Other Helpers #
  #################

  defp emit_messages(html, errors) do
    Earmark.Global.Messages.add_messages(errors)
    html
  end
end
