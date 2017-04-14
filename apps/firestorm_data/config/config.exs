# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :firestorm_data,
  ecto_repos: [FirestormData.Repo]

import_config "#{Mix.env}.exs"
