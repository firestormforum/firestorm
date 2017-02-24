defmodule Phoenix.HTML do
  @moduledoc """
  Helpers for working with HTML strings and templates.

  When used, it imports the given modules:

    * `Phoenix.HTML`- functions to handle HTML safety;

    * `Phoenix.HTML.Tag` - functions for generating HTML tags;

    * `Phoenix.HTML.Form` - functions for working with forms;

    * `Phoenix.HTML.Link` - functions for generating links and urls;

    * `Phoenix.HTML.Format` - functions for formatting text;

  ## HTML Safe

  One of the main responsibilities of this module is to
  provide convenience functions for escaping and marking
  HTML code as safe.

  By default, data output in templates is not considered
  safe:

      <%= "<hello>" %>

  will be shown as:

      &lt;hello&gt;

  User data or data coming from the database is almost never
  considered safe. However, in some cases, you may want to tag
  it as safe and show its "raw" contents:

      <%= raw "<hello>" %>

  Keep in mind most helpers will automatically escape your data
  and return safe content:

      <%= content_tag :p, "<hello>" %>

  will properly output:

      <p>&lt;hello&gt;</p>

  """

  @doc false
  defmacro __using__(_) do
    quote do
      import Phoenix.HTML
      import Phoenix.HTML.Form
      import Phoenix.HTML.Link
      import Phoenix.HTML.Tag
      import Phoenix.HTML.Format
    end
  end

  @typedoc "Guaranteed to be safe"
  @type safe    :: {:safe, iodata}

  @typedoc "May be safe or unsafe (i.e. it needs to be converted)"
  @type unsafe  :: Phoenix.HTML.Safe.t

  @doc """
  Provides `~e` sigil with HTML safe EEx syntax inside source files.

  Raises on attempts to interpolate with `\#{}`, so `~E` should be preferred.

      iex> ~e"\""
      ...> Hello <%= "world" %>
      ...> "\""
      {:safe, [[["" | "Hello "] | "world"] | "\\n"]}

  """
  defmacro sigil_e(expr, opts) do
    handle_sigil(expr, opts, __CALLER__.line)
  end

  @doc """
  Provides `~E` sigil with HTML safe EEx syntax inside source files.

  Does not raise on attempts to interpolate with `\#{}`, but rather shows those
  characters literally, so it should be preferred over `~e`.

      iex> ~E"\""
      ...> Hello <%= "world" %>
      ...> "\""
      {:safe, [[["" | "Hello "] | "world"] | "\\n"]}

  """
  defmacro sigil_E(expr, opts) do
    handle_sigil(expr, opts, __CALLER__.line)
  end

  defp handle_sigil({:<<>>, _, [expr]}, [], line) do
    EEx.compile_string(expr, engine: Phoenix.HTML.Engine, line: line + 1)
  end

  defp handle_sigil(_, _, _) do
    raise ArgumentError, "interpolation not allowed in ~e sigil. " <>
                         "Remove the interpolation, use <%= %> to insert values, " <>
                         "or use ~E to show the interpolation literally"
  end

  @doc """
  Marks the given content as raw.

  This means any HTML code inside the given
  string won't be escaped.

      iex> raw("<hello>")
      {:safe, "<hello>"}
      iex> raw({:safe, "<hello>"})
      {:safe, "<hello>"}
      iex> raw(nil)
      {:safe, ""}

  """
  @spec raw(iodata | safe) :: safe
  def raw({:safe, value}), do: {:safe, value}
  def raw(nil), do: {:safe, ""}
  def raw(value) when is_binary(value) or is_list(value), do: {:safe, value}

  @doc """
  Escapes the HTML entities in the given term, returning iodata.

      iex> html_escape("<hello>")
      {:safe, "&lt;hello&gt;"}

      iex> html_escape('<hello>')
      {:safe, ["&lt;", 104, 101, 108, 108, 111, "&gt;"]}

      iex> html_escape(1)
      {:safe, "1"}

      iex> html_escape({:safe, "<hello>"})
      {:safe, "<hello>"}
  """
  @spec html_escape(unsafe) :: safe
  def html_escape({:safe, _} = safe),
    do: safe
  def html_escape(nil),
    do: {:safe, ""}
  def html_escape(bin) when is_binary(bin),
    do: {:safe, Plug.HTML.html_escape(bin)}
  def html_escape(list) when is_list(list),
    do: {:safe, Phoenix.HTML.Safe.List.to_iodata(list)}
  def html_escape(other),
    do: {:safe, Phoenix.HTML.Safe.to_iodata(other)}

  @doc """
  Converts a safe result into a string.

  Fails if the result is not safe. In such cases, you can
  invoke `html_escape/1` or `raw/1` accordingly before.
  """
  @spec safe_to_string(safe) :: String.t
  def safe_to_string({:safe, iodata}) do
    IO.iodata_to_binary(iodata)
  end

  @doc """
  Escapes quotes (double and single), double backslashes and other

  This function is useful in JavaScript responses when there is a need
  to escape html rendered from other templates, like in the following:

      $("#container").append("<%= escape_javascript(render("post.html", post: @post)) %>");
  """
  @spec escape_javascript(binary | safe) :: String.t
  def escape_javascript({:safe, data}) do
    {:safe, data |> IO.iodata_to_binary |> escape_javascript}
  end

  def escape_javascript(data) when is_binary(data) do
    escape_javascript(data, "")
  end

  defp escape_javascript(<<0x2028::utf8, t::binary>>, acc),
    do: escape_javascript(t, <<acc::binary, "&#x2028;">>)
  defp escape_javascript(<<0x2029::utf8, t::binary>>, acc),
    do: escape_javascript(t, <<acc::binary, "&#x2029;">>)
  defp escape_javascript(<<"</", t::binary>>, acc),
    do: escape_javascript(t, <<acc::binary, ?<, ?\\, ?/>>)
  defp escape_javascript(<<"\r\n", t::binary>>, acc),
    do: escape_javascript(t, <<acc::binary, ?\\, ?n>>)
  defp escape_javascript(<<h, t::binary>>, acc) when h in [?", ?', ?\\],
    do: escape_javascript(t, <<acc::binary, ?\\, h>>)
  defp escape_javascript(<<h, t::binary>>, acc) when h in [?\r, ?\n],
    do: escape_javascript(t, <<acc::binary, ?\\, ?n>>)
  defp escape_javascript(<<h, t::binary>>, acc),
    do: escape_javascript(t, <<acc::binary, h>>)
  defp escape_javascript(<<>>, acc),
    do: acc
end
