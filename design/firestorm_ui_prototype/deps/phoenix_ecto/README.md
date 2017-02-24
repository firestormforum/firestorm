A project that integrates [Phoenix](http://github.com/phoenixframework/phoenix) with [Ecto](http://github.com/elixir-lang/ecto), implementing all relevant protocols.

## Usage

You can use `phoenix_ecto` in your projects in two steps:

1. Add it to your `mix.exs` dependencies:

    ```elixir
    def deps do
      [{:phoenix_ecto, "~> 3.0"}]
    end
    ```

2. List it as your application dependency:

    ```elixir
    def application do
      [applications: [:logger, :phoenix_ecto]]
    end
    ```

## Concurrent browser tests

This library also provides a plug called `Phoenix.Ecto.SQL.Sandbox` that allows developers to run acceptance tests powered by headless browsers such as Phantom.js and Selenium concurrently. If you are not familiar with Ecto's SQL sandbox, we recommend you to first get acquainted with it by [reading `Ecto.Adapters.SQL.Sandbox` documentation](https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html).

To enable concurrent acceptance tests, make sure you are using PostgreSQL and follow the instructions below:

  1. Set a flag to enable the sandbox in `config/test.exs`:

    ```elixir
    config :your_app, sql_sandbox: true
    ```

  2. And use the flag to conditionally add the plug to `lib/your_app/endpoint.ex`:

    ```elixir
    if Application.get_env(:your_app, :sql_sandbox) do
      plug Phoenix.Ecto.SQL.Sandbox
    end
    ```

    Make sure that this is placed **before** the line `plug YourApp.Router` (or any other plug that may access the database).

You can now checkout a sandboxed connection and pass the connection information to an acceptance testing tool like [Hound](https://github.com/hashnuke/hound) or [Wallaby](https://github.com/keathley/wallaby).

### Hound

To write concurrent acceptance tests with Hound, first add it as a dependency to your `mix.exs`:

```elixir
{:hound, "~> 1.0"}
```

Make sure to start it at the top of your `test/test_helper.exs`:

```elixir
{:ok, _} = Application.ensure_all_started(:hound)
```

Then add the following to your test case (or to your case template):

```elixir
use Hound.Helpers

setup do
  :ok = Ecto.Adapters.SQL.Sandbox.checkout(YourApp.Repo)
  metadata = Phoenix.Ecto.SQL.Sandbox.metadata_for(YourApp.Repo, self())
  Hound.start_session(metadata: metadata)
end
```

Hound supports multiple drivers like Chrome, Firefox, etc but it does not support concurrent tests under PhantomJS (the default).

### Wallaby

To write concurrent acceptance tests with Wallaby, first add it as a dependency to your `mix.exs`:

```elixir
{:wallaby, "~> 0.6"}
```

Make sure to start it at the top of your `test/test_helper.exs`:

```elixir
{:ok, _} = Application.ensure_all_started(:wallaby)
```

Then add the following to your test case (or to your case template):

```elixir
use Wallaby.DSL

setup do
  :ok = Ecto.Adapters.SQL.Sandbox.checkout(YourApp.Repo)
  metadata = Phoenix.Ecto.SQL.Sandbox.metadata_for(YourApp.Repo, self())
  {:ok, session} = Wallaby.start_session(metadata: metadata)
end
```

Wallaby currently supports PhantomJS (including concurrent tests). Support for other drivers may be added in the future.

## The Phoenix <-> Ecto integration

Thanks to Elixir protocols, the integration between Phoenix and Ecto is simply a matter of implementing a handful of protocols. We provide the following implementations:

  * `Phoenix.HTML.FormData` protocol for `Ecto.Changeset`
  * `Phoenix.HTML.Safe` protocol for `Decimal`, `Ecto.Date`, `Ecto.Time` and `Ecto.DateTime`
  * `Plug.Exception` protocol for the relevant Ecto exceptions

## License

Same license as Phoenix.
