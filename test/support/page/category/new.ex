defmodule Page.Category.New do
  import Wallaby.Query

  def title_field(), do: text_field("category_title")
  def create_category_button(), do: button("Create Category")
end
