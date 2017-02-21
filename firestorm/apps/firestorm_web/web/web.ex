defmodule FirestormWeb.Web do
  @moduledoc """
  A module defining __using__ hooks for controllers,
  views and so on.

  This can be used in your application as:

      use FirestormWeb.Web, :controller
      use FirestormWeb.Web, :view

  The definitions below will be executed for every view,
  controller, etc, so keep them short and clean, focused
  on imports, uses and aliases.

  Do NOT define functions inside the quoted expressions
  below.
  """

  def model do
    quote do
      # Define common model functionality
    end
  end

  def controller do
    quote do
      use Phoenix.Controller

      import FirestormWeb.Router.Helpers
      import FirestormWeb.Gettext
      import Ecto.Query, only: [from: 2]
      alias FirestormData.{Category, Post, Thread, User, Repo}

      # Import session helpers
      import FirestormWeb.Session, only: [current_user: 1, logged_in?: 1]
    end
  end

  def view do
    quote do
      use Phoenix.View, root: "web/templates"

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_csrf_token: 0, get_flash: 2, get_flash: 1, view_module: 1]

      # Import session helpers
      import FirestormWeb.Session, only: [current_user: 1, logged_in?: 1]

      # Use all HTML functionality (forms, tags, etc)
      use Phoenix.HTML

      import FirestormWeb.Router.Helpers
      import FirestormWeb.ErrorHelpers
      import FirestormWeb.Gettext
    end
  end

  def router do
    quote do
      use Phoenix.Router
    end
  end

  def channel do
    quote do
      use Phoenix.Channel
      import FirestormWeb.Gettext
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
