defmodule Page.Thread.Show do
  import Wallaby.Query

  def thread_title(title), do: css("h2", text: title)
end
