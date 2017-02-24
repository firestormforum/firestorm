defmodule Phoenix.HTML.Tag do
  @moduledoc ~S"""
  Helpers related to producing HTML tags within templates.
  """

  import Phoenix.HTML
  import Plug.CSRFProtection, only: [get_csrf_token: 0]

  @tag_prefixes [:aria, :data]
  @csrf_param "_csrf_token"
  @method_param "_method"

  @doc ~S"""
  Creates an HTML tag with the given name and options.

      iex> tag(:br)
      {:safe, [60, "br", [], 62]}
      iex> tag(:input, type: "text", name: "user_id")
      {:safe, [60, "input", [[32, "name", 61, 34, "user_id", 34], [32, "type", 61, 34, "text", 34]], 62]}

  ## Data attributes

  In order to add custom data attributes you need to pass
  a tuple containing :data atom and a keyword list
  with data attributes' names and values as the first element
  in the tag's attributes keyword list:

      iex> tag(:input, [{:data, [foo: "bar"]}, id: "some_id"])
      {:safe, [60, "input", [[32, "data-foo", 61, 34, "bar", 34], [32, "id", 61, 34, "some_id", 34]], 62]}

  ## Boolean values

  In case an attribute contains a boolean value, its key
  is repeated when it is true, as expected in HTML, or
  the attribute is completely removed if it is false:

      iex> tag(:audio, autoplay: true)
      {:safe, [60, "audio", [[32, "autoplay", 61, 34, "autoplay", 34]], 62]}
      iex> tag(:audio, autoplay: false)
      {:safe, [60, "audio", [], 62]}

  If you want the boolean attribute to be sent as is,
  you can explicitly convert it to a string before.
  """
  def tag(name), do: tag(name, [])
  def tag(name, attrs) when is_list(attrs) do
    {:safe, [?<, to_string(name), build_attrs(name, attrs), ?>]}
  end

  @doc ~S"""
  Creates an HTML tag with given name, content, and attributes.

      iex> content_tag(:p, "Hello")
      {:safe, [60, "p", [], 62, "Hello", 60, 47, "p", 62]}
      iex> content_tag(:p, "<Hello>", class: "test")
      {:safe, [60, "p", [[32, "class", 61, 34, "test", 34]], 62, "&lt;Hello&gt;", 60, 47, "p", 62]}

      iex> content_tag :p, class: "test" do
      ...>   "Hello"
      ...> end
      {:safe, [60, "p", [[32, "class", 61, 34, "test", 34]], 62, "Hello", 60, 47, "p", 62]}
  """
  def content_tag(name, [do: block]) when is_atom(name) do
    content_tag(name, block, [])
  end

  def content_tag(name, content) when is_atom(name) do
    content_tag(name, content, [])
  end

  def content_tag(name, attrs, [do: block]) when is_atom(name) and is_list(attrs) do
    content_tag(name, block, attrs)
  end

  def content_tag(name, content, attrs) when is_atom(name) and is_list(attrs) do
    name = to_string(name)
    {:safe, escaped} = html_escape(content)
    {:safe, [?<, name, build_attrs(name, attrs), ?>, escaped, ?<, ?/, name, ?>]}
  end

  defp tag_attrs([]), do: []
  defp tag_attrs(attrs) do
    for {k, v} <- attrs do
      [?\s, k, ?=, ?", attr_escape(v), ?"]
    end
  end

  defp attr_escape({:safe, data}),
    do: data
  defp attr_escape(nil),
    do: []
  defp attr_escape(other) when is_binary(other),
    do: Plug.HTML.html_escape(other)
  defp attr_escape(other),
    do: Phoenix.HTML.Safe.to_iodata(other)

  defp nested_attrs(attr, dict, acc) do
    Enum.reduce dict, acc, fn {k,v}, acc ->
      attr_name = "#{attr}-#{dasherize(k)}"
      case is_list(v) do
        true  -> nested_attrs(attr_name, v, acc)
        false -> [{attr_name, v}|acc]
      end
    end
  end

  defp build_attrs(_tag, []), do: []
  defp build_attrs(tag, attrs), do: build_attrs(tag, attrs, [])

  defp build_attrs(_tag, [], acc),
    do: acc |> Enum.sort |> tag_attrs
  defp build_attrs(tag, [{k, v}|t], acc) when k in @tag_prefixes and is_list(v) do
    build_attrs(tag, t, nested_attrs(dasherize(k), v, acc))
  end
  defp build_attrs(tag, [{k, true}|t], acc) do
    k = dasherize(k)
    build_attrs(tag, t, [{k, k}|acc])
  end
  defp build_attrs(tag, [{_, false}|t], acc) do
    build_attrs(tag, t, acc)
  end
  defp build_attrs(tag, [{_, nil}|t], acc) do
    build_attrs(tag, t, acc)
  end
  defp build_attrs(tag, [{k, v}|t], acc) do
    build_attrs(tag, t, [{dasherize(k), v}|acc])
  end

  defp dasherize(value) when is_atom(value),   do: dasherize(Atom.to_string(value))
  defp dasherize(value) when is_binary(value), do: String.replace(value, "_", "-")

  @doc ~S"""
  Generates a form tag.

  This function generates the `<form>` tag without its
  closing part. Check `form_tag/3` for generating an
  enclosing tag.

  ## Examples

      form_tag("/hello")
      <form action="/hello" method="post">

      form_tag("/hello", method: :get)
      <form action="/hello" method="get">

  ## Options

    * `:method` - the HTTP method. If the method is not "get" nor "post",
      an input tag with name `_method` is generated along-side the form tag.
      Defaults to "post".

    * `:multipart` - when true, sets enctype to "multipart/form-data".
      Required when uploading files

    * `:csrf_token` - for "post" requests, the form tag will automatically
      include an input tag with name `_csrf_token`. When set to false, this
      is disabled

    * `:enforce_utf8` - when false, does not enforce utf8. Read below
      for more information

  All other options are passed to the underlying HTML tag.

  ## Enforce UTF-8

  Although forms provide the `accept-charset` attribute, which we set
  to UTF-8, Internet Explorer 5 up to 8 may ignore the value of this
  attribute if the user chooses their browser to do so. This ends up
  triggering the browser to send data in a format that is not
  understandable by the server.

  For this reason, Phoenix automatically includes a "_utf8=✓" parameter
  in your forms, to force those browsers to send the data in the proper
  encoding. This technique has been seen in the Rails web framework and
  reproduced here.
  """
  def form_tag(action, opts \\ [])

  def form_tag(action, do: block) do
    form_tag(action, [], do: block)
  end

  def form_tag(action, opts) when is_list(opts) do
    {:safe, method} = html_escape(Keyword.get(opts, :method, "post"))

    {opts, extra} =
      case method do
        "get"  -> {opts, ""}
        "post" -> csrf_token_tag(Keyword.put(opts, :method, "post"), "")
        _      -> csrf_token_tag(Keyword.put(opts, :method, "post"),
                                 ~s'<input name="#{@method_param}" type="hidden" value="#{method}">')
      end

    {opts, extra} =
      case Keyword.pop(opts, :enforce_utf8, true) do
        {false, opts} -> {opts, extra}
        {true, opts}  -> {Keyword.put_new(opts, :accept_charset, "UTF-8"),
                          extra <> ~s'<input name="_utf8" type="hidden" value="✓">'}
      end

    opts =
      case Keyword.pop(opts, :multipart, false) do
        {false, opts} -> opts
        {true, opts}  -> Keyword.put(opts, :enctype, "multipart/form-data")
      end

    html_escape [tag(:form, [action: action] ++ opts), raw(extra)]
  end

  @doc """
  Generates a form tag with the given contents.

  ## Examples

      form_tag("/hello", method: "get") do
        "Hello"
      end
      <form action="/hello" method="get">...Hello...</form>

  """
  def form_tag(action, options, do: block) do
    html_escape [form_tag(action, options), block, raw("</form>")]
  end

  defp csrf_token_tag(opts, extra) do
    case Keyword.pop(opts, :csrf_token, true) do
      {true, opts} ->
        {opts, extra <> ~s'<input name="#{@csrf_param}" type="hidden" value="#{get_csrf_token()}">'}
      {false, opts} ->
        {opts, extra}
    end
  end

  @doc """
  Generates a meta tag with CSRF information.

  ## Tag attributes

    * `content` - a valid csrf token
    * `csrf-param` - a request parameter where expected csrf token
    * `method-param` - a request parameter where expected a custom HTTP method

  """
  def csrf_meta_tag do
    tag :meta, charset: "UTF-8", name: "csrf-token", content: get_csrf_token(),
               'csrf-param': @csrf_param, 'method-param': @method_param
  end

  @doc """
  Generates an img tag with a src.

  ## Examples

      img_tag(user.photo_path)
      <img src="photo.png">

      img_tag(user.photo, class: "image")
      <img src="smile.png" class="image">

  """
  def img_tag(src, opts \\ []) do
    tag(:img, Keyword.put_new(opts, :src, src))
  end
end
