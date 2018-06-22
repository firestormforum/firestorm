defmodule FirestormWeb.Web.Policy do
  @moduledoc """
  Contains helpers for interacting with the policy. Things like "translate an error reason into a user-facing string."
  """

  def translate_policy_reason(:unauthorized) do
    "You are not authorized to perform this action."
  end

  def translate_policy_reason(:unauthenticated) do
    "You must be logged in to perform this action."
  end
end
