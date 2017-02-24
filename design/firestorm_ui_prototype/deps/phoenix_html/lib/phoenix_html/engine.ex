defmodule Phoenix.HTML.Engine do
  @moduledoc """
  This is an implementation of EEx.Engine that guarantees
  templates are HTML Safe.
  """

  @anno (if :erlang.system_info(:otp_release) >= '19' do
    [generated: true]
  else
    [line: -1]
  end)

  use EEx.Engine

  @doc false
  def init(_opts), do: {:safe, ""}

  @doc false
  def handle_body(body), do: body

  @doc false
  def handle_text("", text) do # Required for Elixir < v1.3
    handle_text({:safe, ""}, text)
  end

  def handle_text({:safe, buffer}, text) do
    quote do
      {:safe, [unquote(buffer)|unquote(text)]}
    end
  end

  @doc false
  def handle_expr("", marker, expr) do # Required for Elixir < v1.3
    handle_expr({:safe, ""}, marker, expr)
  end

  def handle_expr({:safe, buffer}, "=", expr) do
    line   = line_from_expr(expr)
    expr   = expr(expr)

    {:safe, quote do
      tmp1 = unquote(buffer)
      [tmp1|unquote(to_safe(expr, line))]
     end}
  end

  def handle_expr({:safe, buffer}, "", expr) do
    expr = expr(expr)

    {:safe, quote do
      tmp2 = unquote(buffer)
      unquote(expr)
      tmp2
    end}
  end

  defp line_from_expr({_, meta, _}) when is_list(meta), do: Keyword.get(meta, :line)
  defp line_from_expr(_), do: nil

  # We can do the work at compile time
  defp to_safe(literal, _line) when is_binary(literal) or is_atom(literal) or is_number(literal) do
    Phoenix.HTML.Safe.to_iodata(literal)
  end

  # We can do the work at runtime
  defp to_safe(literal, line) when is_list(literal) do
    quote line: line, do: Phoenix.HTML.Safe.to_iodata(unquote(literal))
  end

  # We need to check at runtime and we do so by
  # optimizing common cases.
  defp to_safe(expr, line) do
    # Keep stacktraces for protocol dispatch...
    fallback = quote line: line, do: Phoenix.HTML.Safe.to_iodata(other)

    # However ignore them for the generated clauses to avoid warnings
    quote @anno do
      case unquote(expr) do
        {:safe, data} -> data
        bin when is_binary(bin) -> Plug.HTML.html_escape(bin)
        other -> unquote(fallback)
      end
    end
  end

  defp expr(expr) do
    Macro.prewalk(expr, &handle_assign/1)
  end

  defp handle_assign({:@, meta, [{name, _, atom}]}) when is_atom(name) and is_atom(atom) do
    quote line: meta[:line] || 0 do
      Phoenix.HTML.Engine.fetch_assign(var!(assigns), unquote(name))
    end
  end
  defp handle_assign(arg), do: arg

  @doc false
  def fetch_assign(assigns, key) do
    case Access.fetch(assigns, key) do
      {:ok, val} ->
        val
      :error ->
        raise ArgumentError, message: """
        assign @#{key} not available in eex template.

        Please make sure all proper assigns have been set. If this
        is a child template, ensure assigns are given explicitly by
        the parent template as they are not automatically forwarded.

        Available assigns: #{inspect Enum.map(assigns, &elem(&1, 0))}
        """
    end
  end
end
