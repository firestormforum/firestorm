defmodule FirestormWeb.Forums do
  @moduledoc """
  The boundary for the Forums system.
  """

  use Bodyguard.Policy, policy: FirestormWeb.Forums.Policy
  import Ecto.{Query, Changeset}, warn: false
  alias FirestormWeb.{Repo, Notifications}
  alias FirestormWeb.Forums.{
    User,
    Category,
    Thread,
    Post,
    Watch,
    View,
    Notification,
    OembedExtractor
  }

  @doc """
  Returns the list of users.

  ## Examples

      iex> list_users()
      [%User{}, ...]

  """
  def list_users do
    Repo.all(User)
  end

  def paginate_users(page) do
    User
    |> order_by([p], [desc: p.inserted_at])
    |> Repo.paginate(page: page)
  end

  @doc """
  Gets a single user.

  Raises `Ecto.NoResultsError` if the User does not exist.

  ## Examples

      iex> get_user!(123)
      %User{}

      iex> get_user!(456)
      ** (Ecto.NoResultsError)

  """
  def get_user!(id), do: Repo.get!(User, id)

  @doc """
  Gets a single user by username. Maybe.
  """
  def get_user_by_username(username), do: Repo.get_by(User, %{username: username})

  @doc """
  Gets a single user by api_token. Maybe.
  """
  def get_user_by_api_token(api_token), do: Repo.get_by(User, %{api_token: api_token})

  @doc """
  Gets a single user by email address. Maybe.
  """
  def get_user_by_email(email), do: Repo.get_by(User, %{email: email})

  @doc """
  Creates a user.

  ## Examples

      iex> create_user(%{field: value})
      {:ok, %User{}}

      iex> create_user(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_user(attrs \\ %{}) do
    attrs =
      attrs
      |> Map.put(:api_token, generate_api_token())

    %User{}
    |> user_changeset(attrs)
    |> Repo.insert()
  end

  def register_user(attrs \\ %{}) do
    %User{}
    |> user_registration_changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a user.

  ## Examples

      iex> update_user(user, %{field: new_value})
      {:ok, %User{}}

      iex> update_user(user, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_user(%User{} = user, attrs) do
    user
    |> user_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a User.

  ## Examples

      iex> delete_user(user)
      {:ok, %User{}}

      iex> delete_user(user)
      {:error, %Ecto.Changeset{}}

  """
  def delete_user(%User{} = user) do
    Repo.delete(user)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking user changes.

  ## Examples

      iex> change_user(user)
      %Ecto.Changeset{source: %User{}}

  """
  def change_user(%User{} = user) do
    user_changeset(user, %{})
  end

  defp user_changeset(%User{} = user, attrs) do
    user
    |> cast(attrs, [:username, :email, :name, :api_token])
    |> validate_required([:username, :name, :api_token])
    |> unique_constraint(:username)
  end

  def user_registration_changeset(%User{} = user, attrs) do
    user
    |> user_changeset(attrs)
    |> cast(attrs, [:password])
    |> validate_length(:password, min: 6)
    |> put_password_hash()
  end

  defp put_password_hash(changeset) do
    case changeset do
      %Ecto.Changeset{valid?: true, changes: %{password: pass}} ->
        changeset
        |> put_change(:password_hash, Comeonin.Bcrypt.hashpwsalt(pass))
      _ ->
        changeset
    end
  end

  @doc """
  Returns the list of categories.

  ## Examples

      iex> list_categories()
      [%Category{}, ...]

  """
  def list_categories do
    Category
    |> order_by([asc: :slug])
    |> Repo.all()
  end

  @doc """
  Takes a list of categories and returns them as well as a map of category ids to recent threads.
  """
  def get_recent_threads_for_categories(categories, user) do
    threads =
      Thread
      |> join(:left_lateral, [t], p in fragment("SELECT thread_id, inserted_at FROM forums_posts WHERE forums_posts.thread_id = ? ORDER BY forums_posts.inserted_at DESC LIMIT 1", t.id))
      |> order_by([t, p], [desc: p.inserted_at])
      |> where([t, p], t.category_id in ^(Enum.map(categories, &(&1.id))))
      |> limit(3)
      |> select([t], t)
      |> Repo.all()
      |> Repo.preload(posts: from(p in Post, order_by: p.inserted_at, preload: :user))
      |> decorate_threads(user)

    initial_threads_map =
      for category <- categories, into: %{} do
        {category.id, []}
      end

    threads_map =
      threads
      |> Enum.reduce(initial_threads_map, fn(thread, acc) ->
        Map.update(acc, thread.category_id, [thread], fn(cat_threads) -> cat_threads ++ [thread] end)
      end)

    {categories, threads_map}
  end

  @doc """
  Gets a single category.

  Raises `Ecto.NoResultsError` if the Category does not exist.

  ## Examples

      iex> get_category!(123)
      %Category{}

      iex> get_category!(456)
      ** (Ecto.NoResultsError)

  """
  def get_category!(id), do: Repo.get!(Category, id)

  @doc """
  Creates a category.

  ## Examples

      iex> create_category(%{title: "Elixir"})
      {:ok, %Category{}}

      iex> create_category(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_category(attrs \\ %{}) do
    %Category{}
    |> category_changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a category.

  ## Examples

      iex> update_category(category, %{field: new_value})
      {:ok, %Category{}}

      iex> update_category(category, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_category(%Category{} = category, attrs) do
    category
    |> category_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Category.

  ## Examples

      iex> delete_category(category)
      {:ok, %Category{}}

      iex> delete_category(category)
      {:error, %Ecto.Changeset{}}

  """
  def delete_category(%Category{} = category) do
    Repo.delete(category)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking category changes.

  ## Examples

      iex> change_category(category)
      %Ecto.Changeset{source: %Category{}}

  """
  def change_category(%Category{} = category) do
    category_changeset(category, %{})
  end

  defp category_changeset(%Category{} = category, attrs) do
    alias FirestormWeb.Forums.Slugs.CategoryTitleSlug
    category
    |> cast(attrs, [:title])
    |> validate_required([:title])
    |> CategoryTitleSlug.maybe_generate_slug
    |> CategoryTitleSlug.unique_constraint
  end

  @doc """
  Returns the list of threads for a given category. If provided a user, will
  determine whether each thread has been completely read or not.

  ## Examples

      iex> list_threads(category)
      [%Thread{}, ...]

  """
  def list_threads(category, user \\ nil) do
    Thread
    |> where([t], t.category_id == ^category.id)
    |> preload(posts: :user)
    |> Repo.all
    |> decorate_threads(user)
  end

  @doc """
  Returns the threads in a given category ordered by those with the most recent
  posts. If provided a user, will determine whether each thread has been
  completely read or not.

  ## Examples

      iex> recent_threads(category)
      [%Thread{}, ...]

  """
  def recent_threads(category, user \\ nil) do
    Thread
    |> join(:left_lateral, [t], p in fragment("SELECT thread_id, inserted_at FROM forums_posts WHERE forums_posts.thread_id = ? ORDER BY forums_posts.inserted_at DESC LIMIT 1", t.id))
    |> order_by([t, p], [desc: p.inserted_at])
    |> where(category_id: ^category.id)
    |> select([t], t)
    |> Repo.all
    |> Repo.preload(posts: from(p in Post, order_by: p.inserted_at, preload: :user))
    |> decorate_threads(user)
  end

  # Decorate a list of threads with:
  # - first_post
  # - posts_count
  # - completely_read?
  defp decorate_threads(threads, user) do
    threads
    |> Enum.map(fn(thread) ->
      first_post = Enum.at(thread.posts, 0)
      posts_count = length(thread.posts)
      completely_read? =
        if user do
          # FIXME: This is insanely inefficient, lol?
          thread.posts
          |> Enum.all?(fn(post) -> post |> viewed_by?(user) end)
        else
          false
        end
      %Thread{thread | first_post: first_post, posts_count: posts_count, completely_read?: completely_read?}
    end)
  end

  @doc """
  Gets a single thread in a category.

  Raises `Ecto.NoResultsError` if the Thread does not exist in that category.

  ## Examples

      iex> get_thread!(category, 123)
      %Thread{}

      iex> get_thread!(category, 456)
      ** (Ecto.NoResultsError)

  """
  def get_thread!(category, id) do
    Thread
    |> where([t], t.category_id == ^category.id)
    |> Repo.get!(id)
  end

  @doc """
  Gets a thread by id.

  Maybe returns a thread.
  """
  def get_thread(id) do
    Thread
    |> Repo.get(id)
  end

  @doc """
  Gets a thread by id

  Raises `Ecto.NoResultsError` if the Thread does not exist.

  ## Examples

      iex> get_thread!(123)
      %Thread{}

      iex> get_thread!(456)
      ** (Ecto.NoResultsError)

  """
  def get_thread!(id) do
    Thread
    |> Repo.get!(id)
  end

  @doc """
  Gets a post by id

  Raises `Ecto.NoResultsError` if the Post does not exist.

  ## Examples

      iex> get_post!(123)
      %Post{}

      iex> get_post!(456)
      ** (Ecto.NoResultsError)

  """
  def get_post!(id) do
    Post
    |> Repo.get!(id)
  end

  @doc """
  Creates a thread.

  ## Examples

      iex> create_thread(category, user, %{title: "OTP is cool", body: "don't you think?"})
      {:ok, %Thread{}}

      iex> create_thread(category, user, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_thread(category, user, attrs \\ %{}) do
    post_attrs =
      attrs
      |> Map.take([:body])
      |> Map.put(:user_id, user.id)

    thread_attrs =
      attrs
      |> Map.take([:title])
      |> Map.put(:category_id, category.id)

    %{thread: thread_attrs, post: post_attrs}
    |> new_thread_changeset()
    |> Repo.insert
  end

  defp new_thread_changeset(%{thread: thread_attrs, post: post_attrs}) do
    post_changeset =
      %Post{}
      |> cast(post_attrs, [:body, :user_id])
      |> validate_required([:body, :user_id])

    %Thread{}
    |> thread_changeset(thread_attrs)
    |> put_assoc(:posts, [post_changeset])
  end

  @doc """
  Updates a thread.

  ## Examples

      iex> update_thread(thread, %{field: new_value})
      {:ok, %Thread{}}

      iex> update_thread(thread, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_thread(%Thread{} = thread, attrs) do
    thread
    |> thread_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Thread.

  ## Examples

      iex> delete_thread(thread)
      {:ok, %Thread{}}

      iex> delete_thread(thread)
      {:error, %Ecto.Changeset{}}

  """
  def delete_thread(%Thread{} = thread) do
    Repo.delete(thread)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking thread changes.

  ## Examples

      iex> change_thread(thread)
      %Ecto.Changeset{source: %Thread{}}

  """
  def change_thread(%Thread{} = thread) do
    thread
    |> Repo.preload(:posts)
    |> thread_changeset(%{})
    |> put_assoc(:posts, thread.posts)
  end

  defp thread_changeset(%Thread{} = thread, attrs) do
    alias FirestormWeb.Forums.Slugs.ThreadTitleSlug
    thread
    |> cast(attrs, [:title, :category_id])
    |> validate_required([:title, :category_id])
    |> ThreadTitleSlug.maybe_generate_slug
    |> ThreadTitleSlug.unique_constraint
  end

  def login_or_register_from_github(%{nickname: nickname, name: nil, email: _email} = user) do
    login_or_register_from_github(%{user | name: nickname})
  end

  def login_or_register_from_github(%{nickname: nickname, name: _name, email: nil} = user) do
    login_or_register_from_github(%{user | email: nickname <> "@users.noreply.github.com"})
  end

  def login_or_register_from_github(%{nickname: nickname, name: name, email: email}) do
    case get_user_by_username(nickname) do
      nil ->
        create_user(%{email: email, name: name, username: nickname, api_token: generate_api_token()})
      user ->
        {:ok, user}
    end
  end

  def login_or_register_from_identity(%{username: username, password: password}) do
    import Comeonin.Bcrypt, only: [checkpw: 2]
    alias FirestormWeb.Web.Endpoint
    require Endpoint

    case get_user_by_username(username) do
      nil ->
        # No user, let's register one!
        register_user(%{username: username, name: username, password: password, api_token: generate_api_token()})
      user ->
        # We'll check the password with checkpw against the user's stored
        # password hash
        Endpoint.instrument :pryin, %{key: "Forums.login_or_register_from_identity#checkpw"}, fn ->
          case checkpw(password, user.password_hash) do
            true ->
              # Everything checks out, success
              {:ok, user}
            _ ->
              # User existed, we checked the password, but no dice
              {:error, "No user found with that username or password"}
          end
        end
    end
  end

  @doc """
  Creates a post within a thread.

  ## Examples

      iex> create_post(thread, user, %{body: "don't you think?"})
      {:ok, %Post{}}

      iex> create_post(thread, user, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  # NOTE: This obviously doesn't need to use `with` if I'm not using the
  # else...whatever
  def create_post(%Thread{} = thread, %User{} = user, attrs) do
    attrs =
      attrs
      |> Map.put(:thread_id, thread.id)
      |> Map.put(:user_id, user.id)

    with changeset <- post_changeset(%Post{}, attrs),
         {:ok, post} <- Repo.insert(changeset),
         :ok <- Notifications.post_created(post) do
         {:ok, post}
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking post changes.

  ## Examples

      iex> change_post(post)
      %Ecto.Changeset{source: %Post{}}

  """
  def change_post(%Post{} = post) do
    post
    |> post_changeset(%{})
  end

  defp post_changeset(%Post{} = post, attrs) do
    post
    |> cast(attrs, [:body, :thread_id, :user_id])
    |> validate_required([:body, :thread_id, :user_id])
  end

  def user_posts(user, %{page: page}) do
    Post
    |> where([p], p.user_id == ^user.id)
    |> order_by([p], [desc: p.inserted_at])
    |> preload([p], [thread: [:category], user: []])
    |> Repo.paginate(page: page)
  end

  def user_last_post(user) do
    Post
    |> where([p], p.user_id == ^user.id)
    |> order_by([p], [desc: p.inserted_at])
    |> limit(1)
    |> Repo.one
  end

  # FIXME: Should track when users log in rather than proxying that by
  # pretending them making a view always happens when they're on the site.
  def user_last_seen(user) do
    "forums_posts_views"
    |> where([v], v.user_id == ^user.id)
    |> order_by([v], [desc: v.inserted_at])
    |> select([v], v.inserted_at)
    |> limit(1)
    |> Repo.one
  end

  @doc """
  Have a user watch a thread:

      iex> %User{} |> watch(%Thread{})
      {:ok, %Watch{}}

  """
  def watch(%User{} = user, %Thread{} = thread) do
    thread
    |> Ecto.build_assoc(:watches, %{user_id: user.id})
    |> watch_changeset(%{})
    |> Repo.insert()
  end

  @doc """
  Ensure a user no longer watches a thread:

      iex> %User{} |> unwatch(%Thread{})
      :ok

  """
  def unwatch(%User{} = user, %Thread{} = thread) do
    "forums_threads_watches"
    |> where(assoc_id: ^thread.id)
    |> where(user_id: ^user.id)
    |> Repo.delete_all()

    :ok
  end

  @doc """
  Determine if a user is watching a given watchable (Thread, etc):

      iex> %Thread{} |> watched_by?(%User{})
      false

  """
  def watched_by?(watchable, %User{} = user) do
    watch_count(watchable, user) > 0
  end

  def watcher_ids(watchable) do
    watchable
    |> watches()
    |> select([f], f.user_id)
    |> Repo.all
  end

  def watch_count(watchable) do
    watchable
    |> watches()
    |> Repo.aggregate(:count, :id)
  end
  defp watch_count(watchable, user = %User{}) do
    watchable
    |> watches()
    |> where([f], f.user_id == ^user.id)
    |> Repo.aggregate(:count, :id)
  end

  defp watches(watchable) do
    watchable
    |> Ecto.assoc(:watches)
  end

  def home_threads(user_or_nil) do
    Thread
    |> join(:left_lateral, [t], p in fragment("SELECT thread_id, inserted_at FROM forums_posts WHERE forums_posts.thread_id = ? ORDER BY forums_posts.inserted_at DESC LIMIT 1", t.id))
    |> order_by([t, p], [desc: p.inserted_at])
    |> select([t], t)
    |> Repo.all
    |> Repo.preload(posts: from(p in Post, order_by: p.inserted_at, preload: :user))
    |> Repo.preload(:category)
    |> decorate_threads(user_or_nil)
  end

  def watched_threads(%User{} = user) do
    "forums_threads_watches"
    |> where([w], w.user_id == ^user.id)
    |> select([w], w.assoc_id)
    |> Repo.all()
    |> get_decorated_threads(user)
  end

  def participating_threads(%User{} = user) do
    Post
    |> where([p], p.user_id == ^user.id)
    |> select([p], p.thread_id)
    |> Repo.all()
    |> get_decorated_threads(user)
  end

  defp get_decorated_threads(thread_ids, user) do
    Thread
    |> join(:left_lateral, [t], p in fragment("SELECT thread_id, inserted_at FROM forums_posts WHERE forums_posts.thread_id = ? ORDER BY forums_posts.inserted_at DESC LIMIT 1", t.id))
    |> where([t], t.id in ^thread_ids)
    |> order_by([t, p], [desc: p.inserted_at])
    |> preload([category: [], posts: [:user]])
    |> Repo.all
    |> decorate_threads(user)
  end

  defp watch_changeset(%Watch{} = watch, attrs) do
    watch
    |> cast(attrs, [:assoc_id, :user_id])
    |> validate_required([:assoc_id, :user_id])
  end

  @doc """
  Indicate a user viewed a post:

      iex> %User{} |> view(%Post{})
      {:ok, %Post{}}

  """
  def view(%User{} = user, %Post{} = post) do
    post
    |> Ecto.build_assoc(:views, %{user_id: user.id})
    |> view_changeset(%{})
    |> Repo.insert()
  end

  @doc """
  Determine if a user has viewed a given viewable (Post, etc):

      iex> %Post{} |> viewed_by?(%User{})
      false

  """
  def viewed_by?(viewable, %User{} = user) do
    view_count(viewable, user) > 0
  end

  def view_count(viewable) do
    viewable
    |> views()
    |> Repo.aggregate(:count, :id)
  end
  defp view_count(viewable, user = %User{}) do
    viewable
    |> views()
    |> where([f], f.user_id == ^user.id)
    |> Repo.aggregate(:count, :id)
  end

  defp views(viewable) do
    viewable
    |> Ecto.assoc(:views)
  end

  defp view_changeset(%View{} = view, attrs) do
    view
    |> cast(attrs, [:assoc_id, :user_id])
    |> validate_required([:assoc_id, :user_id])
  end

  def notifications_for(%User{} = user) do
    Notification
    |> where([n], n.user_id == ^user.id)
    |> Repo.all()
  end

  defp notification_changeset(%Notification{} = notification, attrs) do
    notification
    |> cast(attrs, [:user_id, :body, :subject, :url])
    |> validate_required([:user_id, :body, :subject, :url])
  end

  @doc """
  Send a notification to a user:

      iex> %User{} |> notify("Nice shoes")
      {:ok, %Notification{}}

  """
  def notify(%User{} = user, %{subject: subject, body: body, url: url}) do
    %Notification{}
    |> notification_changeset(%{body: body, subject: subject, url: url, user_id: user.id})
    |> Repo.insert()
  end

  @doc """
  Gets a notification by id.

  Maybe returns a notification.
  """
  def get_notification(id) do
    Notification
    |> Repo.get(id)
  end

  def decorate_post_oembeds(%Post{} = post) do
    oembeds =
      post.body
      |> OembedExtractor.get_embeds()

    %Post{ post | oembeds: oembeds }
  end

  defp generate_api_token(), do: UUID.uuid4()
end
