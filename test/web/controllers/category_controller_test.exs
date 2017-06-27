defmodule FirestormWeb.Web.CategoryControllerTest do
  use FirestormWeb.Web.ConnCase

  alias FirestormWeb.Forums

  @create_attrs %{title: "some title"}
  @update_attrs %{title: "some updated title"}
  @invalid_attrs %{title: nil}

  def fixture(:category) do
    {:ok, category} = Forums.create_category(@create_attrs)
    category
  end

  test "lists all entries on index", %{conn: conn} do
    category = fixture(:category)
    conn = get conn, category_path(conn, :index)
    assert html_response(conn, 200) =~ category.title
  end

  test "renders form for new categories", %{conn: conn} do
    conn = get conn, category_path(conn, :new)
    assert html_response(conn, 200) =~ "New Category"
  end

  test "creates category and redirects to show when data is valid", %{conn: conn} do
    conn = post conn, category_path(conn, :create), category: @create_attrs

    assert %{id: id} = redirected_params(conn)
    assert redirected_to(conn) == category_path(conn, :show, id)

    conn = get conn, category_path(conn, :show, id)
    assert html_response(conn, 200) =~ "some title"
  end

  test "does not create category and renders errors when data is invalid", %{conn: conn} do
    conn = post conn, category_path(conn, :create), category: @invalid_attrs
    assert html_response(conn, 200) =~ "Create a new category"
  end

  test "renders form for editing chosen category", %{conn: conn} do
    category = fixture(:category)
    conn = get conn, category_path(conn, :edit, category)
    assert html_response(conn, 200) =~ "Edit Category"
  end

  test "updates chosen category and redirects when data is valid", %{conn: conn} do
    category = fixture(:category)
    conn = put conn, category_path(conn, :update, category), category: @update_attrs
    assert redirected_to(conn) == category_path(conn, :show, category)

    conn = get conn, category_path(conn, :show, category)
    assert html_response(conn, 200) =~ "some updated title"
  end

  test "does not update chosen category and renders errors when data is invalid", %{conn: conn} do
    category = fixture(:category)
    conn = put conn, category_path(conn, :update, category), category: @invalid_attrs
    assert html_response(conn, 200) =~ "Edit Category"
  end

  test "deletes chosen category", %{conn: conn} do
    category = fixture(:category)
    conn = delete conn, category_path(conn, :delete, category)
    assert redirected_to(conn) == category_path(conn, :index)
    assert_error_sent 404, fn ->
      get conn, category_path(conn, :show, category)
    end
  end
end
