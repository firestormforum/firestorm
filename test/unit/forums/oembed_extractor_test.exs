defmodule FirestormWeb.Forums.OembedExtractorTest do
  use ExUnit.Case

  alias FirestormWeb.Forums.OembedExtractor

  test "returns a list of oembed data for a given block of text, by extracting links" do
    example_text = """
    This is a cool video, check it out: https://www.youtube.com/watch?v=H686MDn4Lo8
    """
    empire_city_elixir_conf_vid =
      %OEmbed.Video{author_name: "Empire City Elixir Conference",
        author_url: "https://www.youtube.com/channel/UCIYiFWyuEytDzyju6uXW40Q",
        cache_age: nil, height: 270,
        html: "<iframe width=\"480\" height=\"270\" src=\"https://www.youtube.com/embed/H686MDn4Lo8?feature=oembed\" frameborder=\"0\" allowfullscreen></iframe>",
        provider_name: "YouTube", provider_url: "https://www.youtube.com/",
        thumbnail_height: 360,
        thumbnail_url: "https://i.ytimg.com/vi/H686MDn4Lo8/hqdefault.jpg",
        thumbnail_width: 480, title: "Real World Elixir Deployment // Pete Gamache",
        type: "video", version: "1.0", width: 480}

    assert [empire_city_elixir_conf_vid] == OembedExtractor.get_embeds(example_text)
  end
end
