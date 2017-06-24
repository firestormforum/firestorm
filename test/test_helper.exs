ExUnit.configure(exclude: [pending: true])
ExUnit.start()

Ecto.Adapters.SQL.Sandbox.mode(FirestormWeb.Repo, :manual)
{:ok, _} = Application.ensure_all_started(:wallaby)

Application.put_env(:wallaby, :base_url, FirestormWeb.Web.Endpoint.url)
Application.put_env(:firestorm_web, :get_session_from_cookies, true)
