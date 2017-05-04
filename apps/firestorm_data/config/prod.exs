use Mix.Config

config :firestorm_data, FirestormData.Repo,
  adapter: Ecto.Adapters.Postgres,
  url: System.get_env("DATABASE_URL"),
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
  loggers: [
    {Ecto.LogEntry, :log, []},
    {ScoutApm.Instruments.EctoLogger, :log, []}
  ],
  ssl: true
