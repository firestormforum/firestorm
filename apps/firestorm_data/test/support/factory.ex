defmodule FirestormData.Factory do
  use ExMachina.Ecto, repo: FirestormData.Repo
  alias FirestormData.{Category, Thread, Post, User}

  def category_factory() do
    %Category{
      title: Faker.Lorem.sentence(%Range{first: 1, last: 10}),
    }
  end

  def thread_factory() do
    %Thread{
      title: Faker.Lorem.sentence(%Range{first: 1, last: 10}),
      category: build(:category),
    }
  end

  def user_factory() do
    %User{
      name: Faker.Name.name,
      username: Faker.Internet.user_name,
      email: Faker.Internet.email,
    }
  end

  def post_factory() do
    %Post{
      body: Faker.Lorem.paragraph,
      user: build(:user),
      thread: build(:thread),
    }
  end
end
