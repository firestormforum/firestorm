defmodule FirestormWeb.Web.ThreadControllerTest do
  use FirestormWeb.Web.ConnCase

  alias FirestormWeb.Forums

  @create_attrs %{title: "some title"}
  @update_attrs %{title: "some updated title"}
  @invalid_attrs %{title: nil}

  setup do
    {:ok, category} = Forums.create_category(%{title: "Category"})
    {:ok, category: category}
  end

  def fixture(category, :thread) do
    attrs =
      @create_attrs
      |> Map.put(:category_id, category.id)
    {:ok, thread} = Forums.create_thread(attrs)
    thread
  end

  test "lists all entries on index", %{conn: conn, category: category} do
    conn = get conn, category_thread_path(conn, :index, category)
    assert html_response(conn, 200) =~ "Listing Threads"
  end

  test "renders form for new threads", %{conn: conn, category: category} do
    conn = get conn, category_thread_path(conn, :new, category)
    assert html_response(conn, 200) =~ "New Thread"
  end

  test "creates thread and redirects to show when data is valid", %{conn: conn, category: category} do
    conn = post conn, category_thread_path(conn, :create, category), thread: @create_attrs

    assert %{id: id} = redirected_params(conn)
    assert redirected_to(conn) == category_thread_path(conn, :show, category, id)

    conn = get conn, category_thread_path(conn, :show, category, id)
    assert html_response(conn, 200) =~ "Show Thread"
  end

  test "does not create thread and renders errors when data is invalid", %{conn: conn, category: category} do
    conn = post conn, category_thread_path(conn, :create, category), thread: @invalid_attrs
    assert html_response(conn, 200) =~ "New Thread"
  end

  test "renders form for editing chosen thread", %{conn: conn, category: category} do
    thread = fixture(category, :thread)
    conn = get conn, category_thread_path(conn, :edit, category, thread)
    assert html_response(conn, 200) =~ "Edit Thread"
  end

  test "updates chosen thread and redirects when data is valid", %{conn: conn, category: category} do
    thread = fixture(category, :thread)
    conn = put conn, category_thread_path(conn, :update, category, thread), thread: @update_attrs
    assert redirected_to(conn) == category_thread_path(conn, :show, category, thread)

    conn = get conn, category_thread_path(conn, :show, category, thread)
    assert html_response(conn, 200) =~ "some updated title"
  end

  test "does not update chosen thread and renders errors when data is invalid", %{conn: conn, category: category} do
    thread = fixture(category, :thread)
    conn = put conn, category_thread_path(conn, :update, category, thread), thread: @invalid_attrs
    assert html_response(conn, 200) =~ "Edit Thread"
  end

  test "deletes chosen thread", %{conn: conn, category: category} do
    thread = fixture(category, :thread)
    conn = delete conn, category_thread_path(conn, :delete, category, thread)
    assert redirected_to(conn) == category_thread_path(conn, :index, category)
    assert_error_sent 404, fn ->
      get conn, category_thread_path(conn, :show, category, thread)
    end
  end
end
