defmodule FirestormWeb.Earmark.AutoLinker do
  @moduledoc """
  AutoLinker will run on a string and return the string with any naked links
  wrapped in an `a` tag with the link as both content and href..
  """

  # A RegEx to match any URL that has spaces or newlinex on either side of it.
  @url_regex ~r{(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?}

  def run(body) do
    @url_regex
    |> Regex.replace(body, "<a href=\"\\0\">\\0</a>")
  end
end
