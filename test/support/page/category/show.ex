defmodule Page.Category.Show do
  import Wallaby.Query

  def category_title(title), do: css("h2 a", text: title)
  def new_thread_link(), do: link("New Thread")
  def threads(count), do: css("ol.thread-list li.thread-list-item", count: count)
  def thread_title(title), do: css("a.title", text: title)
  def thread_posts_count(count, opts \\ []) do
    opts
    |> Keyword.get(:completely_read?)
    |> badge_class()
    |> css(text: "#{count}")
  end

  # Argument is "is this completely read?"
  def badge_class(true), do: "#{badge_class(:default)}:not(.-highlight)"
  def badge_class(false), do: "#{badge_class(:default)}.-highlight"
  def badge_class(_), do: ".supplemental .badge-block"
end
