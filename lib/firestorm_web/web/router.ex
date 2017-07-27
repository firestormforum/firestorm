defmodule FirestormWeb.Web.Router do
  use FirestormWeb.Web, :router

  pipeline :browser do
    plug Ueberauth
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug FirestormWeb.Web.Plugs.CurrentUser
    plug FirestormWeb.Web.Plugs.Notifications
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/auth", FirestormWeb.Web do
    pipe_through :browser

    get "/logout", AuthController, :delete
    get "/:provider", AuthController, :request
    get "/:provider/callback", AuthController, :callback
    post "/:provider/callback", AuthController, :callback
    delete "/logout", AuthController, :delete
  end

  scope "/", FirestormWeb.Web do
    pipe_through :browser # Use the default browser stack

    get "/", ThreadController, :recent
    get "/login", AuthController, :login
    resources "/users", UserController, only: [:index, :edit, :update, :show]
    get "/threads/watching", ThreadController, :watching
    get "/threads/participating", ThreadController, :participating
    resources "/categories", CategoryController do
      get "/threads/:id/watch", ThreadController, :watch
      get "/threads/:id/unwatch", ThreadController, :unwatch

      resources "/threads", ThreadController do
        resources "/posts", PostController, only: [:new, :create]
      end
    end
  end

  # API routes
  scope "/api/v1", FirestormWeb.Web.Api.V1 do
    pipe_through :api

    resources "/preview", PreviewController, only: [:create]
    resources "/upload_signature", UploadSignatureController, only: [:create]
  end

  # Inbound email routes
  scope "/inbound", FirestormWeb.Web do
    pipe_through :api

    post "/sendgrid", InboundController, :sendgrid
  end
end
