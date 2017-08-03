defmodule FirestormWeb.Store.ReplenishRequest do
  defstruct categories: [], threads: [], users: [], posts: []

  @typep finder :: integer | String.t

  @type t :: %FirestormWeb.Store.ReplenishRequest{
    categories: list(finder),
    threads: list(finder),
    users: list(finder),
    posts: list(finder),
  }

  def request_category(request, category_id) do
    %__MODULE__{ request | categories: [category_id | request.categories] }
  end

  def request_thread(request, thread_id) do
    %__MODULE__{ request | threads: [thread_id | request.threads] }
  end

  def request_user(request, user_id) do
    %__MODULE__{ request | users: [user_id | request.users] }
  end

  def request_post(request, post_id) do
    %__MODULE__{ request | posts: [post_id | request.posts] }
  end
end
