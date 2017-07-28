defmodule Page.Thread.Show do
  import Wallaby.Query

  def thread_title(title), do: css("h2", text: title)

  def reply_link(), do: action_link("reply")

  def post_item(body), do: css(".post-item", text: body)

  def post_username(username, count \\ 1), do: css(".post-item > .item-metadata > .username", text: username, count: count)

  def watched_icon(), do: css(".fa-eye.-highlight")

  def watch_link(), do: action_link("watch")

  def oembed_for(url) do
    # FIXME: It would be ideal to do this:
    # css(".oembed-for[data-oembed-url='#{url}']")
    # but it failed for dumb reasons so we're just doing this for now.
    css(".oembed-for")
  end

  defp action_link(name) do
    css("#{first_post_actions_selector()} > li.#{name} > a")
  end

  defp first_post_actions_selector() do
    ".first-post > .post-item-actions > .actions"
  end
end
