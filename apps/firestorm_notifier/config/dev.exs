use Mix.Config

config :firestorm_notifier, FirestormNotifier.Mailer,
  adapter: Bamboo.LocalAdapter
