defmodule FirestormWeb.Store.ReplenishRequest do
  defstruct categories: [], threads: [], users: [], posts: []

  @typep finder :: integer | string

  @type t :: %FirestormWeb.Store.ReplenishRequest{
    categories: list(finder),
    threads: list(finder),
    users: list(finder),
    posts: list(finder),
  }
end

