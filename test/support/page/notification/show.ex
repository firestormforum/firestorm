defmodule Page.Notification.Show do
  import Wallaby.Query
  def notification_body(), do: css(".notification-item > .body")
  def notification_view_button(), do: css(".notification-item > .button-primary")
end
