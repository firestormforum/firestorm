defmodule FirestormWeb.OEmbed do
  @cache_name :oembed_cache

  def for(url) do
    case get(url) do
      nil ->
        Task.start(fn() ->
          case OEmbed.for(url) do
            {:ok, result} ->
              LruCache.put(@cache_name, url, result)
              {:ok, result}
            {:error, reason} ->
              {:error, reason}
          end
        end)
        {:error, "Fetching to populate cache, check again later."}
      result ->
        {:ok, result}
    end
  end

  def get(url) do
    LruCache.get(@cache_name, url)
  end
end
