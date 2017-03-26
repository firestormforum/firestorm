defmodule FirestormWeb.Markdown.SanitizerTest do
  use ExUnit.Case
  alias FirestormWeb.Markdown.Sanitizer

  test "Removes non-whitelisted tags" do
    input = ~s(
      <a href="#safe">Safe</a>
      <script></script>
    )
    expected = ~s(<a href="#safe">Safe</a>\n)
    actual = Sanitizer.sanitize(input)

    assert expected == actual
  end

  test "Remove non-whitelisted attributes" do
    input = """
      <a on_click="function(){alert('pwned');}" href="#unsafe">Unsafe</a>
      <h1>Safe</h1>
    """
    expected = ~s(<h1>Safe</h1>\n)
    actual = Sanitizer.sanitize(input)

    assert expected == actual
  end
end
