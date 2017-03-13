defmodule FirestormWeb.MarkdownTest do
  use ExUnit.Case
  alias FirestormWeb.Markdown

  test "renders basic things" do
    assert "<p>foo</p>\n" == Markdown.render("foo")
  end

  test "autolinks URLs" do
    assert "<p><a href=\"http://slashdot.org\">http://slashdot.org</a></p>\n" == Markdown.render("http://slashdot.org")
  end

  test "handles markdown links reasonably" do
    input = "[This is a link](http://slashdot.org)"
    output = "<p><a href=\"http://slashdot.org\">This is a link</a></p>\n"
    assert output == Markdown.render(input)
  end
end
