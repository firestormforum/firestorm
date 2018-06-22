defmodule FirestormWeb.Store.ReplenishResponse do
  defstruct categories: [], threads: [], users: [], posts: []
  alias FirestormWeb.Forum.{Category, Thread, User, Post}

  @type t :: %FirestormWeb.Store.ReplenishResponse{
          categories: list(Category.t()),
          threads: list(Thread.t()),
          users: list(User.t()),
          posts: list(Post.t())
        }
end
