defmodule FirestormWeb.MarkdownTest do
  use ExUnit.Case
  alias FirestormWeb.Markdown

  test "renders basic things" do
    assert "<p>foo</p>\n" == Markdown.render("foo")
  end

  test "renders GFM blocks with our preferred code class prefix" do
    markdown =
      """
      Here's some Elixir code:

      ```elixir
      defmodule Foo do
        def bar, do: "baz"
      end
      ```
      """

    expected =
      """
      <p>Here’s some Elixir code:</p>
      <pre><code class=\"elixir language-elixir\">defmodule Foo do
        def bar, do: "baz"
      end</code></pre>
      """

    assert expected == Markdown.render(markdown)
  end

  test "autolinks URLs" do
    assert "<p><a href=\"http://slashdot.org\">http://slashdot.org</a></p>\n" == Markdown.render("http://slashdot.org")
    assert "<p><a href=\"http://www.slashdot.org\">http://www.slashdot.org</a></p>\n" == Markdown.render("http://www.slashdot.org")
    assert "<p><a href=\"https://slashdot.org\">https://slashdot.org</a></p>\n" == Markdown.render("https://slashdot.org")
    assert "<p><a href=\"https://www.slashdot.org\">https://www.slashdot.org</a></p>\n" == Markdown.render("https://www.slashdot.org")
    # NOTE: This was the source of a bug, discovered on 2017-07-13. Using the
    # source post itself to ensure this one never shows up again.
    source = "My side project is dwarlixir - a madman's attempt to build a Dwarf-Fortress-like game with (currently) a text-based interface in Elixir, with everything being a process. https://github.com/trevoke/dwarlixir\r\n\r\nProfessionally I'm working on https://exeq.com - Elixir back-end! :D"
    assert Regex.match?(~r{<a href="https://github.com/trevoke/dwarlixir">}, Markdown.render(source))
  end

  test "converts words like :poop: into their emoji unicode representation" do
    poop = Exmoji.from_short_name("poop") |> Exmoji.EmojiChar.render
    fire = Exmoji.from_short_name("fire") |> Exmoji.EmojiChar.render
    input = "This :poop::fire: is great!"
    output = "<p>This #{poop}#{fire} is great!</p>\n"
    assert output == Markdown.render(input)
  end
end
