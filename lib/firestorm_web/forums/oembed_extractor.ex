defmodule FirestormWeb.Forums.OembedExtractor do
  @url_regex ~r(https?://[^ $\n]*)

  def get_embeds(body) do
    body
    |> get_urls_from_string()
    |> Task.async_stream(fn url -> OEmbed.for(url) end)
    |> Enum.filter(fn
       {:ok, data} -> true
       _ -> false
    end)
    |> Enum.map(fn {:ok, {:ok, a}} -> a end)
  end

  @doc """
  Gathers anything in the string that looks like a link into a list of links.
  """
  def get_urls_from_string(string) do
    Regex.scan(@url_regex, string)
    |> Enum.map(&hd/1)
  end
end
