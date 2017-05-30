defmodule FirestormWeb.Markdown.EmojiReplacer do
  @moduledoc """
  EmojiReplacer will run on a string and return the string with any emoji marks
  (i.e. :poop:) replaced with their emoji counterpart.
  """

  # A RegEx to match any emoji mark
  @emoji_regex ~r{:([a-z]+):}

  def run(body) do
    @emoji_regex
    |> Regex.replace(body, &emojify_short_name/2)
  end

  def emojify_short_name(whole_match, short_name) do
    case Exmoji.from_short_name(short_name) do
      nil -> whole_match
      emoji -> Exmoji.EmojiChar.render(emoji)
    end
  end
end
