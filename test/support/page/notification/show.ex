defmodule Page.Notification.Show do
  import Wallaby.Query
  def notification_body(), do: css(".notification-body")
end
