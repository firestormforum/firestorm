defmodule Page.Thread.New do
  import Wallaby.Query

  def title_field(), do: text_field("thread_title")
  def body_field(), do: fillable_field("thread_body")
  def create_thread_button(), do: button("Create Thread")
end
