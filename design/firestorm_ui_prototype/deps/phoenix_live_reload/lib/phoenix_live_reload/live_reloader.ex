defmodule Phoenix.LiveReloader do
  @moduledoc """
  Router for live-reload detection in development.

  ## Usage

  Add the `Phoenix.LiveReloader` plug within a `code_reloading?` block
  in your Endpoint, ie:

      if code_reloading? do
        socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
        plug Phoenix.CodeReloader
        plug Phoenix.LiveReloader
      end

  ## Configuration

  For live-reloading in development, add the following `:live_reload`
  configuration to your Endpoint with a list of patterns to watch for changes:

      config :my_app, MyApp.Endpoint,
      ...
      live_reload: [
        patterns: [
          ~r{priv/static/.*(js|css|png|jpeg|jpg|gif)$},
          ~r{web/views/.*(ex)$},
          ~r{web/templates/.*(eex)$}
        ]
      ]


  By default the URL of the live-reload connection will use the browser's
  host and port. To override this, you can pass the `:url` option, ie:

      config :my_app, MyApp.Endpoint,
      ...
      live_reload: [
        url: "ws://localhost:4000",
        patterns: [
          ~r{priv/static/.*(js|css|png|jpeg|jpg|gif)$},
          ~r{web/views/.*(ex)$},
          ~r{web/templates/.*(eex)$}
        ]
      ]

  """

  import Plug.Conn

  @behaviour Plug

  phoenix_path = Application.app_dir(:phoenix, "priv/static/phoenix.js")
  reload_path  = Application.app_dir(:phoenix_live_reload, "priv/static/phoenix_live_reload.js")
  @external_resource phoenix_path
  @external_resource reload_path
  @phoenix_js File.read!(phoenix_path)
  @phoenix_live_reload_js File.read!(reload_path)

  def init(opts) do
    opts
  end

  def call(%Plug.Conn{path_info: ["phoenix", "live_reload", "frame"]} = conn , _) do
    endpoint = conn.private.phoenix_endpoint
    config = endpoint.config(:live_reload)
    url    = config[:url] || endpoint.path("/phoenix/live_reload/socket")

    conn
    |> put_resp_content_type("text/html")
    |> send_resp(200, """
      <html><body>
      <script>
        #{@phoenix_js}
        var socket = new Phoenix.Socket("#{url}")
        #{@phoenix_live_reload_js}
      </script>
      </body></html>
    """)
    |> halt()
  end

  def call(conn, _) do
    endpoint = conn.private.phoenix_endpoint
    patterns = get_in endpoint.config(:live_reload), [:patterns]
    if patterns && patterns != [] do
      before_send_inject_reloader(conn, endpoint)
    else
      conn
    end
  end

  defp before_send_inject_reloader(conn, endpoint) do
    register_before_send conn, fn conn ->
      resp_body = to_string(conn.resp_body)

      if inject?(conn, resp_body) && :code.is_loaded(endpoint) do
        [page | rest] = String.split(resp_body, "</body>")
        body = page <> reload_assets_tag(conn) <> Enum.join(["</body>" | rest], "")
        put_in conn.resp_body, body
      else
        conn
      end
    end
  end

  defp inject?(conn, resp_body) do
    conn
    |> get_resp_header("content-type")
    |> html_content_type?
    |> Kernel.&&(String.contains?(resp_body, "<body"))
  end
  defp html_content_type?([]), do: false
  defp html_content_type?([type | _]), do: String.starts_with?(type, "text/html")

  defp reload_assets_tag(conn) do
    path = conn.private.phoenix_endpoint.path("/phoenix/live_reload/frame")
    """
    <iframe src="#{path}" style="display: none;"></iframe>
    """
  end
end
