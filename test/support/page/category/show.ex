defmodule Page.Category.Show do
  import Wallaby.Query

  def category_title(title), do: css("h2 a", text: title)
  def new_thread_link(), do: link("New Thread")
end
