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
        def bar, do: &quot;baz&quot;
      end</code></pre>
      """

    assert expected == Markdown.render(markdown)
  end

  test "autolinks URLs" do
    assert "<p><a href=\"http://slashdot.org\">http://slashdot.org</a></p>" == Markdown.render("http://slashdot.org")
  end
end
