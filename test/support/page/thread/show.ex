defmodule Page.Thread.Show do
  import Wallaby.Query

  def thread_title(title), do: css("h2", text: title)

  def reply_link(), do: action_link("reply")

  def post_item(body), do: css(".post-item", text: body)

  def post_username(username), do: css(".post-item > .item-metadata > .username", text: username)

  def watched_icon(), do: css(".fa-eye.-highlight")

  def watch_link(), do: action_link("watch")

  defp action_link(name) do
    css("#{first_post_actions_selector()} > li.#{name} > a")
  end

  defp first_post_actions_selector() do
    ".first-post > .post-item-actions > .actions"
  end
end
