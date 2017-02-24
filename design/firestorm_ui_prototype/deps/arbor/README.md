# Arbor

[![Build Status](https://travis-ci.org/coryodaniel/arbor.svg)](https://travis-ci.org/coryodaniel/arbor)
[![Hex Version](http://img.shields.io/hexpm/v/arbor.svg?style=flat)](https://hex.pm/packages/arbor)
[![Hex docs](http://img.shields.io/badge/hex.pm-docs-green.svg?style=flat)](https://hexdocs.pm/arbor)

Ecto adjacency list and tree traversal using CTEs. Arbor uses a `parent_id` field
and CTEs to create simple deep tree-like SQL hierarchies.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed as:

  1. Add `arbor` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:arbor, "~> 1.0.3"}]
    end
    ```

  2. Ensure `arbor` is started before your application:

    ```elixir
    def application do
      [applications: [:arbor]]
    end
    ```


## Benchmarks

Arbor has been [benchmarked](https://github.com/coryodaniel/arbor_bench) on 10mm+ record tables with efficient results:

10,000,000 rows, 25% root
```
Running siblings
	10000 runs
	Total time: 1.793026000000013
	Avg: 1.7930260000000131e-4
Running children
	10000 runs
	Total time: 1.5967949999999786
	Avg: 1.5967949999999787e-4
Running descendants
	10000 runs
	Total time: 2.5418830000000012
	Avg: 2.5418830000000013e-4
Running ancestors
	10000 runs
	Total time: 2.87076499999998
	Avg: 2.87076499999998e-4
```

## Usage

```elixir
defmodule Comment do
  use Ecto.Schema
  # See config options below
  use Arbor.Tree, foreign_key_type: :binary_id

  schema "comments" do
    field :body, :string
    belongs_to :parent, Arbor.Comment

    timestamps
  end  
end
```

All methods return composable Ecto queries. For in depth examples see the [tests](./test/arbor)

### Roots

Returns root level records.

```elixir
roots = Comment.roots
        |> Repo.all
```


### Siblings

Return the siblings of a record.

```elixir
siblings = my_comment
           |> Comment.siblings
           |> Comment.order_by_popularity
           |> Repo.all
```

### ancestors

Returns the entire ancestor (parent's parent's parent, etc) path to the record, but not including the record.

```elixir
descendants = my_comment
              |> Comment.ancestors
              |> Comment.order_by_inserted_at
              |> Repo.all              
```


### Descendants

Returns the entire descendant tree of a record, but not including the record.

```elixir
descendants = my_comment
              |> Comment.descendants
              |> Comment.order_by_inserted_at
              |> Repo.all              
```

### Children

Returns the immediate children of a record.

```elixir
children = my_comment
           |> Comment.children
           |> Repo.all
```

### Parent

Returns the record's parent.

```elixir
parent = my_comment
         |> Comment.parent
         |> Repo.first
```

## Options

* *table_name* - set the table name to use in [CTEs](https://www.postgresql.org/docs/9.1/static/queries-with.html)
* *tree_name* - set the name of the CTE
* *primary_key* - defaults to field from Ecto's `@primary_key`
* *primary_key_type* - defaults to type from Ecto's `@primary_key`
* *foreign_key* - defauts to `:parent_id`
* *foreign_key_type* - defaults to type from Ecto's `@primary_key`
* *orphan_strategy* - defaults to `:nothing` currently unimplemented

## Contributing

You'll need PostgreSQL installed and a user that can create and drop databases.

You can specify it with the environment variable `ARBOR_DB_USER`.

The `mix test` task will drop and create the database for each run.

```elixir
mix test
```
