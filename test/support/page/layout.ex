defmodule Page.Layout do
  import Wallaby.Query

  def error(text), do: alert_box(:error, text)

  def alert_box(type, text) do
    css(".alert-box.-#{type}", text: text)
  end
end
