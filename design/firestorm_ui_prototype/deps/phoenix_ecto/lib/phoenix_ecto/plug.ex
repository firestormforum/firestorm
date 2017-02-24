defimpl Plug.Exception, for: Ecto.CastError do
  def status(_), do: 400
end

defimpl Plug.Exception, for: Ecto.Query.CastError do
  def status(_), do: 400
end

defimpl Plug.Exception, for: Ecto.NoResultsError do
  def status(_), do: 404
end
