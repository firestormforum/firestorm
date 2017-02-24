defmodule SlugifyTest do
  use ExUnit.Case
  doctest Slugify

  test "using protocol fallback to any" do
    assert Slugify.slugify(42) == "42"
    assert Slugify.slugify(3.1415) == "3-1415"
    assert Slugify.slugify("hello") == "hello"
    assert Slugify.slugify(:hello) == "hello"
    assert Slugify.slugify('hello') == "hello"
    assert Slugify.slugify([]) == ""
    assert Slugify.slugify(["hello", "world"]) == "helloworld"
  end

end
