defmodule FirestormWeb.Earmark.AutoLinkerTest do
  use ExUnit.Case
  alias FirestormWeb.Earmark.AutoLinker

  test "replaces a naked URL with a link" do
    body = "http://slashdot.org<br/>"
    expected = "<a href=\"http://slashdot.org\">http://slashdot.org</a><br/>"
    assert AutoLinker.run(body) == expected
  end
end
