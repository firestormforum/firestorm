
defprotocol Slugify do
  @moduledoc "The Slugify protocol can be used to enable slugs for own data structures."

  @fallback_to_any true

  @doc "Returns the slug for the given data."
  def slugify(data)
end

defimpl Slugify, for: Any do

  @doc "Default handler for anything that implements String.Chars Protocol."
  def slugify(data), do: Slugger.slugify(Kernel.to_string data)
end
