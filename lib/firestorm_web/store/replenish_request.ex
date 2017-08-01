defmodule FirestormWeb.Store.ReplenishRequest do
  defstruct categories: [], threads: [], users: [], posts: []

  @typep finder :: integer | string

  @type t :: %FirestormWeb.Store.ReplenishRequest{
    categories: list(finder),
    threads: list(finder),
    users: list(finder),
    posts: list(finder),
  }

  def request_category(request, category_id) do
    %__MODULE__{ request | categories: [category_id | request.categories] }
  end
end

