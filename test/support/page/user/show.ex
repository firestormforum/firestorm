defmodule Page.User.Show do
  import Wallaby.Query

  def user_details(), do: css(".user-details")
  def user_posts(), do: css(".user-posts")
end
