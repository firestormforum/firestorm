# EctoAutoslugField

[![Build Status](https://travis-ci.org/sobolevn/ecto_autoslug_field.svg?branch=master)](https://travis-ci.org/sobolevn/ecto_autoslug_field) [![Coverage Status](https://coveralls.io/repos/github/sobolevn/ecto_autoslug_field/badge.svg?branch=master)](https://coveralls.io/github/sobolevn/ecto_autoslug_field?branch=master) [![Hex Version](https://img.shields.io/hexpm/v/ecto_autoslug_field.svg)](https://hex.pm/packages/ecto_autoslug_field) [![License](http://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)

`ecto_autoslug_field` is a reusable [`Ecto`](https://github.com/elixir-ecto/ecto) library which can automatically create slugs from other fields.

This librabry internaly uses [`slugger`](https://github.com/h4cc/slugger) as a default slug-engine.

You can find the full documentation online: [docs](https://hexdocs.pm/ecto_autoslug_field).

## Installation

```elixir
def deps do
  # installation via hex (version 0.2 only supports Ecto >= 2.0):
  [{:ecto_autoslug_field, "~> 0.2"}]

  # if you want to use github:
  # [{:ecto_autoslug_field, github: "sobolevn/ecto_autoslug_field"}]

  # if you need support for older Ecto versions (< 2.0):
  # [{:ecto_autoslug_field, "0.1.3"}]
end
```

## Options

There are several options to configure.

Required:

- `:to` - represents the slug field name where to `put_change` to

Optional:

- `:from` - represents the source fields from which to build slug, if this option is not set you have to ovveride `get_sources/2` function
- `:always_change` - if this option is set slug will be recreated from the givven sources each time `maybe_generate_slug` function is called

## Functions

- `get_sources/2` - this function is used to get sources for the slug, [docs](https://hexdocs.pm/ecto_autoslug_field/EctoAutoslugField.SlugBase.html#get_sources/2).
- `build_slug/1` - this function is a place to modify the result slug, [docs](https://hexdocs.pm/ecto_autoslug_field/EctoAutoslugField.SlugBase.html#build_slug/1).

## Examples

The simplest example:

```elixir
defmodule NameSlug do
  use EctoAutoslugField.Slug, from: :name, to: :slug
end

defmodule User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :slug, NameSlug.Type
  end

  @required_fields ~w(name)
  @optional_fields ~w(slug)

  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> NameSlug.maybe_generate_slug
    |> NameSlug.unique_constraint
  end
end
```

More complex example:

```elixir
defmodule ComplexSlug do
  use EctoAutoslugField.Slug, to: :slug

  def get_sourses(changeset, _opts) do
    # This function is used to get sources to build slug from:
    base_fields = [:title]

    if get_change(changeset, :breaking, false) do
      base_fields ++ ["breaking"]
    else
      base_fields
    end
  end

  def build_slug(sources) do
    # Custom slug building rule:
    sources
    |> Enum.join("-")
    |> Slugger.slugify_downcase
    |> String.replace("-", "+")
  end
end

defmodule Article do
  use Ecto.Schema
  import Ecto.Changeset

  schema "articles" do
    field :title, :string
    field :breaking, :boolean
    field :slug, ComplexSlug.Type
  end

  @required_fields ~w(title breaking)
  @optional_fields ~w(slug)

  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
    |> ComplexSlug.maybe_generate_slug
    |> ComplexSlug.unique_constraint
  end
end
```

## Changelog

See [CHANGELOG.md](https://github.com/sobolevn/ecto_autoslug_field/blob/master/CHANGELOG.md).

## License

MIT. Please see [LICENSE.md](https://github.com/sobolevn/ecto_autoslug_field/blob/master/LICENSE.md) for licensing details.
