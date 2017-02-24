defmodule DBConnection.Poolboy.Worker do
  # poolboy requires a single argument start_link function for workers. The
  # workers' supervisor will return an incorrect modules list for this
  # worker. However poolboy starts the supervisor underneath itself (a
  # worker) so tools requiring the modules list from child specs will never
  # try to fetch it anyway.
  def start_link({mod, opts}) do
    DBConnection.Connection.start_link(mod, opts, :poolboy)
  end
end
