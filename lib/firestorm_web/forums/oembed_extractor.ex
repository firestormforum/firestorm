defmodule FirestormWeb.Forums.OembedExtractor do
  @url_regex ~r(https?://[^ $\n]*)

  def get_embeds(body) do
    body
    |> get_urls_from_string()
    |> Task.async_stream(fn url -> {url, FirestormWeb.OEmbed.for(url)} end)
    |> Enum.filter(&successful_oembed?/1)
    |> Enum.map(fn {:ok, {url, {:ok, a}}} -> {url, a} end)
  end

  defp successful_oembed?({:ok, {url, {:ok, _data}}}), do: true
  defp successful_oembed?(x) do
    false
  end

  @doc """
  Gathers anything in the string that looks like a link into a list of links.
  """
  def get_urls_from_string(string) do
    Regex.scan(@url_regex, string)
    |> Enum.map(&hd/1)
  end
end
