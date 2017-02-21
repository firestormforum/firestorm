defmodule FirestormWeb.Router do
  use FirestormWeb.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", FirestormWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/home", PageController, :home

    resources "/categories", CategoryController do
      resources "/threads", ThreadController
    end
  end

  # OAuth routes (ueberauth)
  scope "/auth", FirestormWeb do
    pipe_through [:browser]

    get "/:provider", AuthController, :request
    get "/:provider/callback", AuthController, :callback
    post "/:provider/callback", AuthController, :callback
    delete "/logout", AuthController, :delete
  end

  # Other scopes may use custom stacks.
  # scope "/api", FirestormWeb do
  #   pipe_through :api
  # end
end
