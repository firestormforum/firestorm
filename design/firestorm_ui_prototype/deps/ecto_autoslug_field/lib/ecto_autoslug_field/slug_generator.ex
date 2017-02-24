defmodule EctoAutoslugField.SlugGenerator do
  @moduledoc """
  This module works with slugs itself. It is just a wrapper around 'Slugger'.

  It is suited for inner use.
  """

  import Ecto.Changeset, only: [
    put_change: 3,
    get_change: 3,
  ]

  @doc """
  This is a public wrapper around `do_build_slug/1` functions.

  Default slug builder.
  """
  @spec build_slug(Keyword.t) :: String.t
  def build_slug(sources) do
    do_build_slug(sources)
  end

  @doc """
  This function conditionally generates slug.

  This function prepares sources and then calls `do_generate_slug/3`.
  """
  @spec maybe_generate_slug(
    Changeset.t, atom() | list(), Keyword.t) :: Changeset.t
  def maybe_generate_slug(changeset, source, opts) when is_atom(source) do
    source_value = get_field_data(changeset, source, opts)
    do_generate_slug(changeset, source_value, opts)
  end
  def maybe_generate_slug(changeset, sources, opts) do
    cleaned_sources =
      sources
      |> Enum.map(fn(v) -> get_field_data(changeset, v, opts) end)
      |> Enum.filter(fn(v) -> has_value?(v) end)

    do_generate_slug(changeset, cleaned_sources, opts)
  end

  # Private functions:

  defp do_build_slug(source) when is_binary(source) do
    source |> Slugger.slugify_downcase()
  end
  defp do_build_slug(sources) do
    sources |> Enum.join("-") |> Slugger.slugify_downcase()
  end

  defp do_generate_slug(changeset, sources, opts) do
    slug_key = Keyword.get(opts, :to)
    slug_builder = Keyword.get(opts, :slug_builder)
    always_change = Keyword.get(opts, :always_change, false)

    slug_field = Map.get(changeset.data, slug_key)

    if always_change == true or slug_field == nil do
      # We only generate slug on two occasions:
      # 1. `always_change` flag is set to true
      # 2. `slug_field` is not set, meaning it is a first-time-slug
      do_put_change(changeset, slug_key, slug_builder, sources)
    else
      changeset
    end
  end

  defp do_put_change(changeset, _, _, []), do: changeset
  defp do_put_change(changeset, _, _, nil), do: changeset
  defp do_put_change(changeset, slug_key, slug_builder, sources) do
    # `slug_builder` will be called only if the slug-building occasion
    # was met and the `sources` is not empty.
    slug_string = slug_builder.(sources)
    put_change(changeset, slug_key, slug_string)
  end

  defp get_field_data(changeset, source, opts) when is_atom(source) do
    always_change = Keyword.get(opts, :always_change, false)
    source_value = get_change(changeset, source, nil)

    if always_change do
      source_value || Map.get(changeset.data, source)
    else
      source_value
    end
  end
  defp get_field_data(_, source, _) when is_binary(source), do: source

  defp has_value?(nil), do: false
  defp has_value?(string) do
    String.strip(string) != ""
  end
end
