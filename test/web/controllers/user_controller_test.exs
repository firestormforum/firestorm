defmodule FirestormWeb.Web.UserControllerTest do
  use FirestormWeb.Web.ConnCase

  alias FirestormWeb.Forums

  @create_attrs %{email: "some email", name: "some name", username: "some username"}
  @update_attrs %{email: "some updated email", name: "some updated name", username: "some updated username"}
  @invalid_attrs %{email: nil, name: nil, username: nil}

  def fixture(:user) do
    {:ok, user} = Forums.create_user(@create_attrs)
    user
  end

  test "lists all entries on index", %{conn: conn} do
    conn =
      conn
      |> get(user_path(conn, :index))

    assert html_response(conn, 200) =~ "Users"
  end

  test "updates chosen user and redirects when data is valid", %{conn: conn} do
    user = fixture(:user)
    conn =
      conn
      |> log_in_as(user)
      |> put(user_path(conn, :update, user), user: @update_attrs)

    assert redirected_to(conn) == user_path(conn, :show, user)

    conn =
      conn
      |> get(user_path(conn, :show, user))

    assert html_response(conn, 200) =~ "some updated name"
  end

  test "does not update chosen user and renders errors when data is invalid", %{conn: conn} do
    user = fixture(:user)

    conn =
      conn
      |> log_in_as(user)
      |> put(user_path(conn, :update, user), user: @invalid_attrs)

    assert html_response(conn, 200) =~ "Username/Name"
  end
end
