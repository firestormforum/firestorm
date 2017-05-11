defmodule FirestormWeb.Web.Router do
  use FirestormWeb.Web, :router

  pipeline :browser do
    plug Ueberauth
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/auth", FirestormWeb.Web do
    pipe_through :browser

    get "/:provider", AuthController, :request
    get "/:provider/callback", AuthController, :callback
    post "/:provider/callback", AuthController, :callback
    delete "/logout", AuthController, :delete
    get "/logout", AuthController, :delete
  end

  scope "/", FirestormWeb.Web do
    pipe_through :browser # Use the default browser stack

    get "/", CategoryController, :index
    resources "/users", UserController
    resources "/categories", CategoryController do
      resources "/threads", ThreadController
    end
  end
end
