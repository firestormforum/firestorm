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

  test "converts words like :poop: into their emoji unicode representation" do
    poop = Exmoji.from_short_name("poop") |> Exmoji.EmojiChar.render
    fire = Exmoji.from_short_name("fire") |> Exmoji.EmojiChar.render
    input = "This :poop: :fire: is great!"
    output = "<p>This #{poop} #{fire} is great!</p>\n"
    assert output == Markdown.render(input)
  end
end
