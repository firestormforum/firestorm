defmodule FirestormWeb.Markdown.AutoLinkerTest do
  use ExUnit.Case
  alias FirestormWeb.Markdown.AutoLinker

  test "replaces a naked URL with a link" do
    body = "http://slashdot.org<br/>"
    expected = "<a href=\"http://slashdot.org\">http://slashdot.org</a><br/>"
    assert AutoLinker.run(body) == expected
  end

  test "does nothing to a naked URL wrapped in quotes" do
    body = "\"http://slashdot.org\""
    expected = "\"http://slashdot.org\""
    assert AutoLinker.run(body) == expected
  end
end
