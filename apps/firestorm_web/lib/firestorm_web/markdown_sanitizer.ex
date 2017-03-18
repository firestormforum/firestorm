defmodule FirestormWeb.Markdown.Sanitizer do
  alias Floki.HTMLTree

  def sanitize(html) do
    allowed_tags = MapSet.new([
      "a",
      "b",
      "em",
      "h1",
      "h2",
      "h3",
      "h4",
      "h5",
      "h6",
      "i",
      "li",
      "ol",
      "p",
      "strong",
      "u",
      "ul"
    ])
    allowed_attributes = MapSet.new(["href", "id"])

    parsed = Floki.parse(html)
    html_tree = HTMLTree.build(parsed)
    nodes = Map.values(html_tree.nodes)

    bad_nodes = Enum.filter(nodes, fn node ->
      bad_tag? = case Map.get(node, :type) do
        # Text nodes don't have a type
        nil -> false
        type -> not MapSet.member?(allowed_tags, type)
      end

      bad_attributes? = case Map.get(node, :attributes) do
        nil -> false
        [] -> false
        attributes -> Enum.any?(attributes, fn({attribute, _value}) ->
          not MapSet.member?(allowed_attributes, attribute)
        end)
      end

      bad_tag? or bad_attributes?
    end)

    sanitized_tree = Enum.reduce(bad_nodes, html_tree, fn(node, tree) ->
      HTMLTree.delete_node(tree, node)
    end)

    document = Enum.map(sanitized_tree.root_nodes_ids, fn(node_id) ->
      root = Map.get(sanitized_tree.nodes, node_id)
      HTMLTree.to_tuple(sanitized_tree, root)
    end)

    Floki.raw_html(document)
  end
end
