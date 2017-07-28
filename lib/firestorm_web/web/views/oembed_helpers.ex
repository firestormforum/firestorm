defmodule FirestormWeb.Web.OembedHelpers do
  def render_oembed(%OEmbed.Photo{url: url}) do
    Phoenix.HTML.Tag.img_tag(url)
  end
  def render_oembed(%OEmbed.Video{html: html}) do
    Phoenix.HTML.raw(html)
  end
  def render_oembed(%OEmbed.Rich{html: html}) do
    Phoenix.HTML.raw(html)
  end
  def render_oembed(_), do: nil
end
