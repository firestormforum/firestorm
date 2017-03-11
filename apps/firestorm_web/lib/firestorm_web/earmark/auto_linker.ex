defmodule FirestormWeb.Earmark.AutoLinker do
  @moduledoc """
  AutoLinker will run on a string and return the string with any naked links
  wrapped in an `a` tag with the link as both content and href..
  """

  # A RegEx to match any URL that has spaces or newlinex on either side of it.
  @url_regex ~r{(\s|\A\^)*(https?://[^ $\n]*)(\s|$|\z)+}

  def run(body) do
    @url_regex
    |> Regex.replace(body, "<a href=\"\\2\">\\2</a>")
  end
end
