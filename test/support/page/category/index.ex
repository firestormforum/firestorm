defmodule Page.Category.Index do
  import Wallaby.Query

  def new_category_link(), do: link("New Category")
  def categories(count), do: css("ol.category-list li", count: count)
  def category_title(text), do: css("h2.title", text: text)
end
