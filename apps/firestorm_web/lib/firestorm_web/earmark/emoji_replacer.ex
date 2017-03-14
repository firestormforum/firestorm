defmodule FirestormWeb.Earmark.EmojiReplacer do
  @moduledoc """
  EmojiReplacer will run on a string and return the string with any emoji marks
  (i.e. :poop:) replaced with their emoji counterpart.
  """

  # A RegEx to match any emoji mark that has spaces or newlines on either side of it.
  @emoji_regex ~r{([ \n]*|^):([a-z]+):([ \n]+|\Z)}

  def run(body) do
    @emoji_regex
    |> Regex.replace(body, &emojify_short_name/4)
  end

  def emojify_short_name(whole_match, pre, short_name, post) do
    case Exmoji.from_short_name(short_name) do
      nil -> whole_match
      emoji ->
        output = Exmoji.EmojiChar.render(emoji)
        "#{pre}#{output}#{post}"
    end
  end
end
