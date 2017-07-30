defmodule Page.Notification.Index do
  import Wallaby.Query

  def notifications(count), do: css(".notification-list > li", count: count)
  def notification_link(), do: css("a")
end
