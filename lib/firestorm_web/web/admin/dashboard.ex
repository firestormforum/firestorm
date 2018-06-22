defmodule FirestormWeb.ExAdmin.Dashboard do
  use ExAdmin.Register
  alias FirestormWeb.Forums.Post
  alias FirestormWeb.Repo
  import Ecto.Query

  register_page "Dashboard" do
    menu(priority: 1, label: "Dashboard")

    content do
      columns do
        column do
          panel "Recent Posts" do
            Post
            |> Post.ordered()
            |> limit(5)
            |> preload([:thread, :user])
            |> Repo.all()
            |> table_for do
              column("Thread", fn p ->
                a(p.thread.title, href: "/admin/threads/#{p.thread_id}")
              end)

              column("User", fn p -> p.user.username end)

              column("Body", fn p ->
                p.body
                |> String.slice(0..40)
                |> a(href: "/admin/posts/#{p.id}")
              end)
            end
          end
        end
      end
    end
  end
end
