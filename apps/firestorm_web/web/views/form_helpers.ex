defmodule FirestormWeb.FormHelpers do
  use Phoenix.HTML

  # Add on a "pure-button" class any time we use a submit
  def submit(value, opts \\ []) do
    opts =
      opts
      |> Keyword.update(:class, "pure-button", &add_pure_button/1)

    Phoenix.HTML.Form.submit(value, opts)
  end

  defp add_pure_button(classes), do: "#{classes} pure-button"
end
