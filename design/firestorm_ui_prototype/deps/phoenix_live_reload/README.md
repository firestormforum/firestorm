A project for live-reload functionality for [Phoenix](http://github.com/phoenixframework/phoenix) during development.

## Usage

You can use `phoenix_live_reload` in your projects by adding it to your `mix.exs` dependencies:

```elixir
def deps do
  [{:phoenix_live_reload, "~> 1.0"}]
end
```

## Backends

This project uses [`fs`](https://github.com/synrc/fs) as a dependency to watch your filesystem whenever there is a change and it supports the following operating systems:

* Linux via [inotify](https://github.com/rvoicilas/inotify-tools/wiki) (installation required)
* Windows via [inotify-win](https://github.com/thekid/inotify-win) (no installation required)
* Mac OS X via fsevents (no installation required)


## Skipping remote CSS reload

All stylesheets are reloaded without a page refresh anytime a style is detected as having changed. In certain cases such as serving stylesheets from a remote host, you may wish to prevent unnecessary reload of these stylesheets during development. For this, you can include a `data-no-reload` attribute on the link tag, ie:

    <link rel="stylesheet" href="http://example.com/style.css" data-no-reload>


## License

[Same license as Phoenix](https://github.com/phoenixframework/phoenix/blob/master/LICENSE.md).
