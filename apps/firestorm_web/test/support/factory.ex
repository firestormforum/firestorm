defmodule FirestormWeb.Factory do
  use ExMachina.Ecto, repo: FirestormData.Repo
  alias FirestormData.{User, Category, Thread, Post}

  def user_factory do
    user_name = Faker.Internet.user_name
    %User{
      username: user_name,
      email: "#{user_name}@example.org"
    }
  end

  def category_factory do
    %Category{
      title: Faker.Internet.domain_word
    }
  end

  def thread_factory do
    %Thread{
      title: Faker.Internet.domain_word,
      category_id: factory(:category).id
    }
  end

  def post_factory do
    %Post{
      body: Faker.Lorem.paragraph,
      user_id: factory(:user).id,
      thread_id: factory(:thread).id
    }
  end

  def set_title_slug(%{title: title}=thing) do
    %{thing | slug: Slugger.slugify(title)}
  end
end
