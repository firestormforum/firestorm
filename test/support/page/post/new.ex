defmodule Page.Post.New do
  import Wallaby.Query

  def body_field(), do: fillable_field("post_body")
  def reply_button(), do: button("Reply")
end
