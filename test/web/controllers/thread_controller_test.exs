defmodule FirestormWeb.Web.ThreadControllerTest do
  use FirestormWeb.Web.ConnCase

  alias FirestormWeb.Forums

  @create_attrs %{title: "some title"}
  @update_attrs %{title: "some updated title"}
  @invalid_attrs %{title: nil}

  def fixture(:thread) do
    {:ok, thread} = Forums.create_thread(@create_attrs)
    thread
  end

  test "lists all entries on index", %{conn: conn} do
    conn = get conn, thread_path(conn, :index)
    assert html_response(conn, 200) =~ "Listing Threads"
  end

  test "renders form for new threads", %{conn: conn} do
    conn = get conn, thread_path(conn, :new)
    assert html_response(conn, 200) =~ "New Thread"
  end

  test "creates thread and redirects to show when data is valid", %{conn: conn} do
    conn = post conn, thread_path(conn, :create), thread: @create_attrs

    assert %{id: id} = redirected_params(conn)
    assert redirected_to(conn) == thread_path(conn, :show, id)

    conn = get conn, thread_path(conn, :show, id)
    assert html_response(conn, 200) =~ "Show Thread"
  end

  test "does not create thread and renders errors when data is invalid", %{conn: conn} do
    conn = post conn, thread_path(conn, :create), thread: @invalid_attrs
    assert html_response(conn, 200) =~ "New Thread"
  end

  test "renders form for editing chosen thread", %{conn: conn} do
    thread = fixture(:thread)
    conn = get conn, thread_path(conn, :edit, thread)
    assert html_response(conn, 200) =~ "Edit Thread"
  end

  test "updates chosen thread and redirects when data is valid", %{conn: conn} do
    thread = fixture(:thread)
    conn = put conn, thread_path(conn, :update, thread), thread: @update_attrs
    assert redirected_to(conn) == thread_path(conn, :show, thread)

    conn = get conn, thread_path(conn, :show, thread)
    assert html_response(conn, 200) =~ "some updated title"
  end

  test "does not update chosen thread and renders errors when data is invalid", %{conn: conn} do
    thread = fixture(:thread)
    conn = put conn, thread_path(conn, :update, thread), thread: @invalid_attrs
    assert html_response(conn, 200) =~ "Edit Thread"
  end

  test "deletes chosen thread", %{conn: conn} do
    thread = fixture(:thread)
    conn = delete conn, thread_path(conn, :delete, thread)
    assert redirected_to(conn) == thread_path(conn, :index)
    assert_error_sent 404, fn ->
      get conn, thread_path(conn, :show, thread)
    end
  end
end
