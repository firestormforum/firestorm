defmodule FirestormWeb.Web.Api.V1.PostView do
  use FirestormWeb.Web, :view
  alias FirestormWeb.Forums.Post
  alias FirestormWeb.Markdown

  def render("show.json", %Post{id: id, body: body, inserted_at: inserted_at, updated_at: updated_at, thread_id: thread_id, user_id: user_id, oembeds: oembeds}) do
    %{
      id: id,
      body: body,
      body_html: Markdown.render(body),
      user_id: user_id,
      oembeds: Enum.map(oembeds || [], &do_render_oembed/1),
      inserted_at: inserted_at,
      updated_at: updated_at,
      thread_id: thread_id
    }
  end

  def do_render_oembed({url, oembed}) do
    {:safe, html} = FirestormWeb.Web.OembedHelpers.render_oembed(oembed)
    [ url, "#{html}" ]
  end
end
