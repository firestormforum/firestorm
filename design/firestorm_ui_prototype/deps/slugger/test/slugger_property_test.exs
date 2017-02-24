defmodule SluggerPropertyTest do
  use ExUnit.Case, async: false
  use ExCheck

  property :always_same_slug_created do
    for_all s in unicode_string do
      assert Slugger.slugify(s) == Slugger.slugify(s)
    end
  end

end
