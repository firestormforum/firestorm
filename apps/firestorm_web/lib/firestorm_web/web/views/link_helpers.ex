defmodule FirestormWeb.Web.LinkHelpers do
  @moduledoc """
  View helpers for linking to things easily
  """

  import FirestormWeb.Web.Router.Helpers
  import FirestormWeb.Web.SlugHelpers
  import Phoenix.HTML.{Link, Tag}

  @doc """
  Link to a thread using the thread's title as the link text
  """
  def thread_title_link(conn, thread) do
    link thread.title, to: category_thread_path(conn, :show, category_finder(thread.category), thread.id), class: "title"
  end

  @doc """
  Link to a category using the category's title as the link text
  """
  def category_title_link(conn, category) do
    link category.title, to: category_path(conn, :show, category_finder(category))
  end

  def user_link(user) do
    link "@#{user.username}", to: "#", class: "user-name"
  end
end
