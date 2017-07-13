defmodule FirestormWeb.Markdown.AutoLinker do
  @moduledoc """
  AutoLinker will run on a string and return the string with any naked links
  wrapped in an `a` tag with the link as both content and href..
  """

  # A RegEx to match any URL that has spaces or newlines on either side of it.
  @url_regex ~r{([ \n]+|^)(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?}

  def run(body) do
    @url_regex
    |> Regex.replace(body, "\\1<a href=\"\\2\://\\3\\4\\5\">\\2://\\3\\4\\5</a>")
  end
end
