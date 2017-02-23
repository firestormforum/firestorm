defmodule FirestormWeb.FormHelpers do
  # Add on a "pure-button" class any time we use a submit
  def submit([do: _] = block_option) do
    submit([class: "pure-button"], block_option)
  end

  def submit(_, opts \\ [])
  def submit(opts, [do: _] = block_option) do
    opts =
      opts
      |> Keyword.update(:class, "pure-button", &add_pure_button/1)

    Phoenix.HTML.Form.submit(opts, block_option)
  end
  def submit(value, opts) do
    opts =
      opts
      |> Keyword.update(:class, "pure-button", &add_pure_button/1)

    Phoenix.HTML.Form.submit(value, opts)
  end

  defp add_pure_button(classes), do: "#{classes} pure-button"
end
