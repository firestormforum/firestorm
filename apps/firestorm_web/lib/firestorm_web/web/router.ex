defmodule FirestormWeb.Web.Router do
  use FirestormWeb.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug FirestormWeb.Plugs.CurrentUser
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", FirestormWeb.Web do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/home", PageController, :home

    resources "/categories", CategoryController do
      post "/tag", CategoryController, :tag
      resources "/threads", ThreadController do
        post "/tag", ThreadController, :tag
        get "/follow", ThreadController, :follow
        get "/unfollow", ThreadController, :unfollow
        resources "/posts", PostController
      end
    end
  end

  # OAuth routes (ueberauth)
  scope "/auth", FirestormWeb.Web do
    pipe_through [:browser]

    get "/:provider", AuthController, :request
    get "/:provider/callback", AuthController, :callback
    post "/:provider/callback", AuthController, :callback
    delete "/logout", AuthController, :delete
    get "/logout", AuthController, :delete
  end

  # API routes
  scope "/api/v1", FirestormWeb.Web.Api.V1 do
    pipe_through :api

    get "/home", HomeController, :index
    resources "/categories", CategoryController
  end

  # Inbound email routes
  scope "/inbound", FirestormWeb.Web do
    pipe_through :api

    post "/sendgrid", InboundController, :sendgrid
  end

  # Bamboo dev mail server
  if Mix.env == :dev do
    forward "/sent_emails", Bamboo.EmailPreviewPlug
  end
end
