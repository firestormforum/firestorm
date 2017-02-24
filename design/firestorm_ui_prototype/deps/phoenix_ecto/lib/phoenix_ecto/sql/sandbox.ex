defmodule Phoenix.Ecto.SQL.Sandbox do
  @moduledoc """
  A plug to allow concurrent, transactional acceptance tests with Ecto.Adapters.SQL.Sandbox.

  ## Example

  This plug should only be used during tests. First, set a flag to
  enable it in `config/test.exs`:

      config :your_app, sql_sandbox: true

  And use the flag to conditionally add the plug to `lib/your_app/endpoint.ex`:

      if Application.get_env(:your_app, :sql_sandbox) do
        plug Phoenix.Ecto.SQL.Sandbox
      end
  
  It's important that this is at the top of `endpoint.ex`, before any other plugs.

  Then, within an acceptance test, checkout a sandboxed connection as before.
  Use `metadata_for/2` helper to get the session metadata to that will allow access
  to the test's connection.
  Here's an example using [Hound](https://hex.pm/packages/hound):

      use Hound.Helpers

      setup do
        :ok = Ecto.Adapters.SQL.Sandbox.checkout(YourApp.Repo)
        metadata = Phoenix.Ecto.SQL.Sandbox.metadata_for(YourApp.Repo, self())
        Hound.start_session(metadata: metadata)
      end
  """

  import Plug.Conn

  def init(opts \\ []) do
    Keyword.get(opts, :sandbox, Ecto.Adapters.SQL.Sandbox)
  end

  def call(conn, sandbox) do
    conn
    |> get_req_header("user-agent")
    |> List.first
    |> extract_metadata
    |> allow_sandbox_access(sandbox)

    conn
  end

  @doc """
  Returns metadata to associate with the session
  to allow the endpoint to acces the database connection checked
  out by the test process.
  """
  @spec metadata_for(Ecto.Repo.t | [Ecto.Repo.t], pid) :: map
  def metadata_for(repo_or_repos, pid) when is_pid(pid) do
    %{repo: repo_or_repos, owner: pid}
  end

  defp allow_sandbox_access(%{repo: repo, owner: owner}, sandbox) do
    Enum.each(List.wrap(repo), &sandbox.allow(&1, owner, self()))
  end
  defp allow_sandbox_access(_metadata, _sandbox), do: nil

  defp extract_metadata(user_agent) when is_binary(user_agent) do
    ua_last_part = user_agent |> String.split("/") |> List.last
    case Regex.run(~r/BeamMetadata \((.*?)\)/, ua_last_part) do
      [_, metadata] -> parse_metadata(metadata)
      _             -> %{}
    end
  end
  defp extract_metadata(_), do: %{}

  defp parse_metadata(encoded_metadata) do
    encoded_metadata
    |> Base.url_decode64!
    |> :erlang.binary_to_term
    |> case do
         {:v1, metadata} -> metadata
         _               -> %{}
       end
  end
end
