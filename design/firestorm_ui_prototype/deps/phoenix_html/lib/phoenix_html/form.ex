defmodule Phoenix.HTML.Form do
  @moduledoc ~S"""
  Helpers related to producing HTML forms.

  The functions in this module can be used in three
  distinct scenarios:

    * with changeset data - when information to populate
      the form comes from a changeset

    * with connection data - when a form is created based
      on the information in the connection (aka `Plug.Conn`)

    * without form data - when the functions are used directly,
      outside of a form

  We will explore all three scenarios below.

  ## With changeset data

  The entry point for defining forms in Phoenix is with
  the `form_for/4` function. For this example, we will
  use `Ecto.Changeset`, which integrate nicely with Phoenix
  forms via the `phoenix_ecto` package.

  Imagine you have the following action in your controller:

      def new(conn, _params) do
        changeset = User.changeset(%User{})
        render conn, "new.html", changeset: changeset
      end

  where `User.changeset/2` is defined as follows:

      def changeset(user, params \\ %{}) do
        cast(user, params, [:name, :age])
      end

  Now a `@changeset` assign is available in views which we
  can pass to the form:

      <%= form_for @changeset, user_path(@conn, :create), fn f -> %>
        <label>
          Name: <%= text_input f, :name %>
        </label>

        <label>
          Age: <%= select f, :age, 18..100 %>
        </label>

        <%= submit "Submit" %>
      <% end %>

  `form_for/4` receives the `Ecto.Changeset` and converts it
  to a form, which is passed to the function as the argument
  `f`. All the remaining functions in this module receive
  the form and automatically generate the input fields, often
  by extracting information from the given changeset. For example,
  if the user had a default value for age set, it will
  automatically show up as selected in the form.

  ## With connection data

  `form_for/4` expects as first argument any data structure that
  implements the `Phoenix.HTML.FormData` protocol. By default,
  Phoenix implements this protocol for `Plug.Conn`, allowing us
  to create forms based only on connection information.

  This is useful when you are creating forms that are not backed
  by any kind of data from the data layer, like a search form.

      <%= form_for @conn, search_path(@conn, :new), [as: :search], fn f -> %>
        <%= text_input f, :for %>
        <%= submit "Search" %>
      <% end %>

  `form_for/4` uses the `Plug.Conn` to set input values from the
  request parameters. In this case, the input's value would be set
  to `@conn.params["search"]["for"]`.

  ## Without form data

  Sometimes we may want to generate a `text_input/3` or any other
  tag outside of a form. The functions in this module also support
  such usage by simply passing an atom as first argument instead
  of the form.

      <%= text_input :user, :name, value: "This is a prepopulated value" %>

  ## Nested inputs

  If your data layer supports embedding or nested associations,
  you can use `inputs_for` to attach nested data to the form.

  Imagine the following Ecto schemas:

      defmodule User do
        use Ecto.Schema

        schema "users" do
          field :name
          embeds_one :permalink, Permalink
        end
      end

      defmodule Permalink do
        use Ecto.Schema

        embedded_schema do
          field :url
        end
      end

  In the form, you now can:

      <%= form_for @changeset, user_path(@conn, :create), fn f -> %>
        <%= text_input f, :name %>

        <%= inputs_for f, :permalink, fn fp -> %>
          <%= text_input fp, :url %>
        <% end %>
      <% end %>

  The default option can be given to populate the fields if none
  is given:

      <%= inputs_for f, :permalink, [default: %Permalink{title: "default"}], fn fp -> %>
        <%= text_input fp, :url %>
      <% end %>

  `inputs_for/4` can be used to work with single entities or
  collections. When working with collections, `:prepend` and
  `:append` can be used to add entries to the collection
  stored in the changeset.
  """

  alias Phoenix.HTML.Form
  import Phoenix.HTML
  import Phoenix.HTML.Tag

  @doc """
  Defines the Phoenix.HTML.Form struct.

  Its fields are:

    * `:source` - the data structure given to `form_for/4` that
      implements the form data protocol

    * `:impl` - the module with the form data protocol implementation.
      This is used to avoid multiple protocol dispatches.

    * `:id` - the id to be used when generating input fields

    * `:name` - the name to be used when generating input fields

    * `:data` - the field used to store lookup data

    * `:params` - the parameters associated to this form in case
      they were sent as part of a previous request

    * `:hidden` - a keyword list of fields that are required for
      submitting the form behind the scenes as hidden inputs

    * `:options` - a copy of the options given when creating the
      form via `form_for/4` without any form data specific key

    * `:errors` - a keyword list of errors that associated with
      the form
  """
  defstruct source: nil, impl: nil, id: nil, name: nil, data: nil,
            hidden: [], params: %{}, errors: [], options: [], index: nil

  @type t :: %Form{source: Phoenix.HTML.FormData.t, name: String.t, data: %{atom => term},
                   params: %{binary => term}, hidden: Keyword.t, options: Keyword.t,
                   errors: Keyword.t, impl: module, id: String.t, index: nil | non_neg_integer}

  @doc """
  Converts an attribute/form field into its humanize version.

      iex> humanize(:username)
      "Username"
      iex> humanize(:created_at)
      "Created at"
      iex> humanize("user_id")
      "User"

  """
  def humanize(atom) when is_atom(atom),
    do: humanize(Atom.to_string(atom))
  def humanize(bin) when is_binary(bin) do
    bin =
      if String.ends_with?(bin, "_id") do
        binary_part(bin, 0, byte_size(bin) - 3)
      else
        bin
      end

    bin |> String.replace("_", " ") |> String.capitalize
  end

  @doc """
  Generates a form tag with a form builder.

  See the module documentation for examples of using this function.

  ## Options

    * `:as` - the server side parameter in which all params for this
      form will be collected (i.e. `as: :user_params` would mean all fields
      for this form will be accessed as `conn.params.user_params` server
      side). Automatically inflected when a changeset is given.

    * `:method` - the HTTP method. If the method is not "get" nor "post",
      an input tag with name `_method` is generated along-side the form tag.
      Defaults to "post".

    * `:multipart` - when true, sets enctype to "multipart/form-data".
      Required when uploading files

    * `:csrf_token` - for "post" requests, the form tag will automatically
      include an input tag with name `_csrf_token`. When set to false, this
      is disabled

    * `:enforce_utf8` - the form will have an `accept-charset` attribute set
      to "UTF-8" and a hidden input named `_utf8` containing a unicode
      character to force the browser to use UTF-8 as the charset. When set to
      false, this is disabled.
     
    * Other options will be passed as html attributes.
      ie, `class: "foo", id: "bar"`

  See `Phoenix.HTML.Tag.form_tag/2` for more information on the
  options above.
  """
  @spec form_for(Phoenix.HTML.FormData.t, String.t,
                 Keyword.t, (t -> Phoenix.HTML.unsafe)) :: Phoenix.HTML.safe
  def form_for(form_data, action, options \\ [], fun) when is_function(fun, 1) do
    form = Phoenix.HTML.FormData.to_form(form_data, options)
    html_escape [form_tag(action, form.options), fun.(form), raw("</form>")]
  end

  @doc """
  Generate a new form builder for the given parameter in form.

  See the module documentation for examples of using this function.

  ## Options

    * `:id` - the id to be used in the form, defaults to the
      concatenation of the given `field` to the parent form id

    * `:as` - the name to be used in the form, defaults to the
      concatenation of the given `field` to the parent form name

    * `:default` - the value to use if none is available

    * `:prepend` - the values to prepend when rendering. This only
      applies if the field value is a list and no parameters were
      sent through the form.

    * `:append` - the values to append when rendering. This only
      applies if the field value is a list and no parameters were
      sent through the form.

  """
  @spec inputs_for(t, atom, Keyword.t, (t -> Phoenix.HTML.unsafe)) :: Phoenix.HTML.safe
  def inputs_for(%{impl: impl} = form, field, options \\ [], fun) do
    options =
      form.options
      |> Keyword.take([:multipart])
      |> Keyword.merge(options)

    forms = impl.to_form(form.source, form, field, options)

    html_escape Enum.map(forms, fn form ->
      hidden = Enum.map(form.hidden, fn {k, v} -> hidden_input(form, k, value: v) end)
      [hidden, fun.(form)]
    end)
  end

  @doc """
  Returns a value of a corresponding form field.

  The `form` should either be a `Phoenix.HTML.Form` emitted
  by `form_for` or an atom.

  When a form is given, it will lookup for changes and then
  fallback to parameters and finally fallback to the default
  struct/map value.
  """
  def input_value(%{source: source, impl: impl} = form, field) when is_atom(field) do
    try do
      impl.input_value(source, form, field)
    rescue
      UndefinedFunctionError ->
        case Map.fetch(form.params, Atom.to_string(field)) do
          {:ok, value} ->
            value
          :error ->
            Map.get(form.data, field)
        end
    end
  end

  def input_value(name, _field) when is_atom(name),
    do: nil

  @doc """
  Returns an id of a corresponding form field.

  The form should either be a `Phoenix.HTML.Form` emitted
  by `form_for` or an atom.
  """
  def input_id(%{id: id}, field),
    do: "#{id}_#{field}"
  def input_id(name, field) when is_atom(name),
    do: "#{name}_#{field}"

  @doc """
  Returns a name of a corresponding form field.

  The form should either be a `Phoenix.HTML.Form` emitted
  by `form_for` or an atom.
  """
  def input_name(%{name: name}, field),
    do: "#{name}[#{field}]"
  def input_name(name, field) when is_atom(name),
    do: "#{name}[#{field}]"

  @doc """
  Returns the HTML5 validations that would apply to
  the given field.
  """
  @spec input_validations(t, atom) :: Keyword.t
  def input_validations(%{source: source, impl: impl} = form, field) do
    # TODO: Remove me on 3.0
    try do
      impl.input_validations(source, form, field)
    rescue
      UndefinedFunctionError -> impl.input_validations(source, field)
    end
  end

  @mapping %{
    "url"      => :url_input,
    "email"    => :email_input,
    "search"   => :search_input,
    "password" => :password_input
  }

  @doc """
  Gets the input type for a given field.

  If the underlying input type is a `:text_field`,
  a mapping could be given to further inflect
  the input type based solely on the field name.
  The default mapping is:

      %{"url"      => :url_input,
        "email"    => :email_input,
        "search"   => :search_input,
        "password" => :password_input}

  """
  @spec input_type(t, atom) :: atom
  def input_type(%{impl: impl, source: source} = form, field, mapping \\ @mapping) do
    type =
      # TODO: Remove me on 3.0
      try do
        impl.input_type(source, form, field)
      rescue
        UndefinedFunctionError -> impl.input_type(source, field)
      end

    if type == :text_input do
      field = Atom.to_string(field)
      Enum.find_value(mapping, type, fn {k, v} ->
        String.contains?(field, k) && v
      end)
    else
      type
    end
  end

  ## Form helpers

  @doc """
  Generates a text input.

  The form should either be a `Phoenix.HTML.Form` emitted
  by `form_for` or an atom.

  All given options are forwarded to the underlying input,
  default values are provided for id, name and value if
  possible.

  ## Examples

      # Assuming form contains a User schema
      text_input(form, :name)
      #=> <input id="user_name" name="user[name]" type="text" value="">

      text_input(:user, :name)
      #=> <input id="user_name" name="user[name]" type="text" value="">

  """
  def text_input(form, field, opts \\ []) do
    generic_input(:text, form, field, opts)
  end

  @doc """
  Generates a hidden input.

  See `text_input/3` for example and docs.
  """
  def hidden_input(form, field, opts \\ []) do
    generic_input(:hidden, form, field, opts)
  end

  @doc """
  Generates an email input.

  See `text_input/3` for example and docs.
  """
  def email_input(form, field, opts \\ []) do
    generic_input(:email, form, field, opts)
  end

  @doc """
  Generates a number input.

  See `text_input/3` for example and docs.
  """
  def number_input(form, field, opts \\ []) do
    generic_input(:number, form, field, opts)
  end

  @doc """
  Generates a password input.

  For security reasons, the form data and parameter values
  are never re-used in `password_input/3`. Pass the value
  explicitly if you would like to set one.

  See `text_input/3` for example and docs.
  """
  def password_input(form, field, opts \\ []) do
    opts =
      opts
      |> Keyword.put_new(:type, "password")
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))
    tag(:input, opts)
  end

  @doc """
  Generates an url input.

  See `text_input/3` for example and docs.
  """
  def url_input(form, field, opts \\ []) do
    generic_input(:url, form, field, opts)
  end

  @doc """
  Generates a search input.

  See `text_input/3` for example and docs.
  """
  def search_input(form, field, opts \\ []) do
    generic_input(:search, form, field, opts)
  end

  @doc """
  Generates a telephone input.

  See `text_input/3` for example and docs.
  """
  def telephone_input(form, field, opts \\ []) do
    generic_input(:tel, form, field, opts)
  end

  @doc """
  Generates a color input.

  Warning: this feature isn't available in all browsers.
  Check `http://caniuse.com/#feat=input-color` for further informations.

  See `text_input/3` for example and docs.
  """
  def color_input(form, field, opts \\ []) do
    generic_input(:color, form, field, opts)
  end

  @doc """
  Generates a range input.

  See `text_input/3` for example and docs.
  """
  def range_input(form, field, opts \\ []) do
    generic_input(:range, form, field, opts)
  end

  defp generic_input(type, form, field, opts) when is_atom(field) and is_list(opts) do
    opts =
      opts
      |> Keyword.put_new(:type, type)
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))
      |> Keyword.put_new(:value, input_value(form, field))
    tag(:input, opts)
  end

  @doc """
  Generates a textarea input.

  All given options are forwarded to the underlying input,
  default values are provided for id, name and textarea
  content if possible.

  ## Examples

      # Assuming form contains a User schema
      textarea(form, :description)
      #=> <textarea id="user_description" name="user[description]"></textarea>

  ## New lines

  Notice the generated textarea includes a new line after
  the opening tag. This is because the HTML spec says new
  lines after tags must be ignored and all major browser
  implementations do that.

  So in order to avoid new lines provided by the user
  from being ignored when the form is resubmitted, we
  automatically add a new line before the text area
  value.
  """
  def textarea(form, field, opts \\ []) do
    opts =
      opts
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))

    {value, opts} = Keyword.pop(opts, :value, input_value(form, field) || "")
    content_tag(:textarea, html_escape(["\n", value]), opts)
  end

  @doc """
  Generates a file input.

  It requires the given form to be configured with `multipart: true`
  when invoking `form_for/4`, otherwise it fails with `ArgumentError`.

  See `text_input/3` for example and docs.
  """
  def file_input(form, field, opts \\ []) do
    if match?(%Form{}, form) and !form.options[:multipart] do
      raise ArgumentError, "file_input/3 requires the enclosing form_for/4 " <>
                           "to be configured with multipart: true"
    end

    opts =
      opts
      |> Keyword.put_new(:type, :file)
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))

    opts =
      if opts[:multiple] do
        Keyword.update!(opts, :name, &("#{&1}[]"))
      else
        opts
      end

    tag(:input, opts)
  end

  @doc """
  Generates a submit button to send the form.

  All options are forwarded to the underlying button tag.

  ## Examples

      submit "Submit"
      #=> <button type="submit">Submit</button>

  """
  def submit([do: _] = block_option), do: submit([], block_option)

  def submit(_, opts \\ [])
  def submit(opts, [do: _] = block_option) do
    opts = Keyword.put_new(opts, :type, "submit")

    content_tag(:button, opts, block_option)
  end

  def submit(value, opts) do
    opts = Keyword.put_new(opts, :type, "submit")

    content_tag(:button, value, opts)
  end

  @doc """
  Generates a reset input to reset all the form fields to
  their original state.

  All options are forwarded to the underlying input tag.

  ## Examples

      reset "Reset"
      #=> <input type="reset" value="Reset">

      reset "Reset", class: "btn"
      #=> <input type="reset" value="Reset" class="btn">

  """
  def reset(value, opts \\ []) do
    opts =
      opts
      |> Keyword.put_new(:type, "reset")
      |> Keyword.put_new(:value, value)
    tag(:input, opts)
  end

  @doc """
  Generates a radio button.

  Invoke this function for each possible value you to be
  sent to the server.

  ## Examples

      # Assuming form contains a User schema
      radio_button(form, :role, "admin")
      #=> <input id="user_role_admin" name="user[role]" type="radio" value="admin">

  ## Options

  All options are simply forwarded to the underlying HTML tag.
  """
  def radio_button(form, field, value, opts \\ []) do
    value = html_escape(value)

    opts =
      opts
      |> Keyword.put_new(:type, "radio")
      |> Keyword.put_new(:id, input_id(form, field) <> "_" <> elem(value, 1))
      |> Keyword.put_new(:name, input_name(form, field))

    opts =
      if value == html_escape(input_value(form, field)) do
        Keyword.put_new(opts, :checked, true)
      else
        opts
      end

    tag(:input, [value: value] ++ opts)
  end

  @doc """
  Generates a checkbox.

  This function is useful for sending boolean values to the server.

  ## Examples

      # Assuming form contains a User schema
      checkbox(form, :famous)
      #=> <input name="user[famous]" type="hidden" value="false">
          <input checked="checked" id="user_famous" name="user[famous]" type="checkbox" value="true">

  ## Options

    * `:checked_value` - the value to be sent when the checkbox is checked.
      Defaults to "true"

    * `:unchecked_value` - the value to be sent when the checkbox is unchecked,
      Defaults to "false"

    * `:value` - the value used to check if a checkbox is checked or unchecked.
      The default value is extracted from the form data if available

  All other options are forwarded to the underlying HTML tag.

  ## Hidden fields

  Because an unchecked checkbox is not sent to the server, Phoenix
  automatically generates a hidden field with the unchecked_value
  *before* the checkbox field to ensure the `unchecked_value` is sent
  when the checkbox is not marked.
  """
  def checkbox(form, field, opts \\ []) do
    opts =
      opts
      |> Keyword.put_new(:type, "checkbox")
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))

    {value, opts}           = Keyword.pop(opts, :value, input_value(form, field))
    {checked_value, opts}   = Keyword.pop(opts, :checked_value, true)
    {unchecked_value, opts} = Keyword.pop(opts, :unchecked_value, false)

    # We html escape all values to be sure we are comparing
    # apples to apples. After all we may have true in the data
    # but "true" in the params and both need to match.
    value           = html_escape(value)
    checked_value   = html_escape(checked_value)
    unchecked_value = html_escape(unchecked_value)

    opts =
      if value == checked_value do
        Keyword.put_new(opts, :checked, true)
      else
        opts
      end

    html_escape [tag(:input, name: Keyword.get(opts, :name), type: "hidden", value: unchecked_value),
                 tag(:input, [value: checked_value] ++ opts)]
  end

  @doc """
  Generates a select tag with the given `options`.

  Values are expected to be an Enumerable containing two-item tuples
  (like a regular list or a map) or any Enumerable where the element
  will be used both as key and value for the generated select.

  ## Optgroups

  When a two-item map contains a list as its value,
  the options will be wrapped in an `<optroup>` using the key as its label.

  ## Examples

      # Assuming form contains a User schema
      select(form, :age, 0..120)
      #=> <select id="user_age" name="user[age]">
          <option value="0">0</option>
          ...
          <option value="120">120</option>
          </select>

      select(form, :role, ["Admin": "admin", "User": "user"])
      #=> <select id="user_role" name="user[role]">
          <option value="admin">Admin</option>
          <option value="user">User</option>
          </select>

      select(form, :role, ["Admin": "admin", "User": "user"], prompt: "Choose your role")
      #=> <select id="user_role" name="user[role]">
          <option value="">Choose your role</option>
          <option value="admin">Admin</option>
          <option value="user">User</option>
          </select>

      select(form, :country, %{"Europe" => ["UK", "Sweden", "France"], …})
      #=> <select id="user_country" name="user[country]">
          <optgroup label="Europe">
            <option>UK</option>
            <option>Sweden</option>
            <option>France</option>
          </optgroup>
          ...
          </select>

      # Assuming users is a list of User schemas and form contains a Resource Schema
      select(form, :user_id, users |> Enum.map(&{&1.name, &1.id}))
      #=> <select id="user_id" name="resource[user_id]">
          <option value="1">Ylva</option>
          <option value="2">Annora</option>
          </select>

  ## Options

    * `:prompt` - an option to include at the top of the options with
      the given prompt text

    * `:selected` - the default value to use when none was sent as parameter

  All other options are forwarded to the underlying HTML tag.
  """
  def select(form, field, options, opts \\ []) do
    {selected, opts} = selected(form, field, opts) || []

    {prefix, opts} = case Keyword.pop(opts, :prompt) do
      {nil, opts}    -> {raw(""), opts}
      {prompt, opts} -> {content_tag(:option, prompt, value: ""), opts}
    end

    opts =
      opts
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field))

    options = options_for_select(options, prefix, html_escape(selected))
    content_tag(:select, options, opts)
  end

  defp selected(form, field, opts) do
    {value, opts} = Keyword.pop(opts, :value)
    {selected, opts} = Keyword.pop(opts, :selected)

    if value != nil do
      {value, opts}
    else
      param = Atom.to_string(field)

      case form do
        %{params: %{^param => sent}} ->
          {sent, opts}
        _ ->
          {selected || input_value(form, field), opts}
      end
    end
  end

  defp options_for_select(values, options, value) do
    Enum.reduce values, options, fn
      {option_key, option_value}, acc ->
        option(option_key, option_value, value, acc)
      option, acc ->
        option(option, option, value, acc)
    end
  end

  defp option(group_label, group_values, value, acc) when is_list(group_values) or is_map(group_values) do
    section_options = options_for_select(group_values, [], value)
    tag = content_tag(:optgroup, section_options, label: group_label)
    html_escape [acc|tag]
  end

  defp option(option_key, option_value, value, acc) do
    option_key   = html_escape(option_key)
    option_value = html_escape(option_value)

    selected =
      if is_list(value) do
        option_value in value
      else
        value == option_value
      end

    opts = [value: option_value, selected: selected]
    html_escape [acc|content_tag(:option, option_key, opts)]
  end

  @doc """
  Generates a select tag with the given `options`.

  Values are expected to be an Enumerable containing two-item tuples
  (like maps and keyword lists) or any Enumerable where the element
  will be used both as key and value for the generated select.

  ## Examples

      # Assuming form contains a User schema
      multiple_select(form, :roles, ["Admin": 1, "Power User": 2])
      #=> <select id="user_roles" name="user[roles][]">
          <option value="1">Admin</option>
          <option value="2">Power User</option>
          </select>

      multiple_select(form, :roles, ["Admin": 1, "Power User": 2], selected: [1])
      #=> <select id="user_roles" name="user[roles]">
          <option value="1" selected="selected" >Admin</option>
          <option value="2">Power User</option>
          </select>

  When working with structs, associations and embeds, you will need to tell
  Phoenix how to extract the value out of the collection. For example,
  imagine `user.roles` is a list of `%Role{}` structs. You must call it as:

      multiple_select(form, :roles, ["Admin": 1, "Power User": 2],
                      selected: Enum.map(@user.roles, &(&1.id))

  The `:selected` option will mark the given IDs as selected unless the form
  is being resubmitted. When resubmitted, it uses the form params as values.

  ## Options

    * `:selected` - the default options to be marked as selected. The values
       on this list are ignored in case ids have been set as parameters.

  All other options are forwarded to the underlying HTML tag.
  """
  def multiple_select(form, field, options, opts \\ []) do
    {selected, opts} = selected(form, field, opts)

    opts =
      opts
      |> Keyword.put_new(:id, input_id(form, field))
      |> Keyword.put_new(:name, input_name(form, field) <> "[]")
      |> Keyword.put_new(:multiple, "")

    options = options_for_select(options, "", Enum.map(List.wrap(selected), &html_escape/1))
    content_tag(:select, options, opts)
  end

  ## Datetime

  @doc ~S"""
  Generates select tags for datetime.

  ## Examples

      # Assuming form contains a User schema
      datetime_select form, :born_at
      #=> <select id="user_born_at_year" name="user[born_at][year]">...</select> /
          <select id="user_born_at_month" name="user[born_at][month]">...</select> /
          <select id="user_born_at_day" name="user[born_at][day]">...</select> —
          <select id="user_born_at_hour" name="user[born_at][hour]">...</select> :
          <select id="user_born_at_min" name="user[born_at][minute]">...</select>

  If you want to include the seconds field (hidden by default), pass `second: []`:

      # Assuming form contains a User schema
      datetime_select form, :born_at, second: []

  If you want to configure the years range:

      # Assuming form contains a User schema
      datetime_select form, :born_at, year: [options: 1900..2100]

  You are also able to configure `:month`, `:day`, `:hour`, `:minute` and
  `:second`. All options given to those keys will be forwarded to the
  underlying select. See `select/4` for more information.

  ## Options

    * `:value` - the value used to select a given option.
      The default value is extracted from the form data if available

    * `:default` - the default value to use when none was given in
      `:value` and none is available in the form data

    * `:year`, `:month`, `:day`, `:hour`, `:minute`, `:second` - options passed
      to the underlying select. See `select/4` for more information.
      The available values can be given in `:options`.

    * `:builder` - specify how the select can be build. It must be a function
      that receives a builder that should be invoked with the select name
      and a set of options. See builder below for more information.

  ## Builder

  The generated datetime_select can be customized at will by providing a
  builder option. Here is an example from EEx:

      <%= datetime_select form, :born_at, builder: fn b -> %>
        Date: <%= b.(:day, []) %> / <%= b.(:month, []) %> / <%= b.(:year, []) %>
        Time: <%= b.(:hour, []) %> : <%= b.(:minute, []) %>
      <% end %>

  Although we have passed empty lists as options (they are required), you
  could pass any option there and it would be given to the underlying select
  input.

  In practice, we recommend you to create your own helper with your default
  builder:

      def my_datetime_select(form, field, opts \\ []) do
        builder = fn b ->
          ~e"\""
          Date: <%= b.(:day, []) %> / <%= b.(:month, []) %> / <%= b.(:year, []) %>
          Time: <%= b.(:hour, []) %> : <%= b.(:minute, []) %>
          "\""
        end

        datetime_select(form, field, [builder: builder] ++ opts)
      end

  Then you are able to use your own datetime_select throughout your whole
  application.

  ## Supported date values

  The following values are supported as date:

    * a map containing the `year`, `month` and `day` keys (either as strings or atoms)
    * a tuple with three elements: `{year, month, day}`
    * `nil`

  ## Supported time values

  The following values are supported as time:

    * a map containing the `hour` and `min` keys and an optional `sec` key (either as strings or atoms)
    * a tuple with three elements: `{hour, min, sec}`
    * a tuple with four elements: `{hour, min, sec, usec}`
    * `nil`

  """
  def datetime_select(form, field, opts \\ []) do
    value = Keyword.get(opts, :value, input_value(form, field) || Keyword.get(opts, :default))

    builder =
      Keyword.get(opts, :builder) || fn b ->
        date = date_builder(b, opts)
        time = time_builder(b, opts)
        html_escape [date, raw(" &mdash; "), time]
      end

    builder.(datetime_builder(form, field, date_value(value), time_value(value), opts))
  end

  @doc """
  Generates select tags for date.

  Check `datetime_select/3` for more information on options and supported values.
  """
  def date_select(form, field, opts \\ []) do
    value   = Keyword.get(opts, :value, input_value(form, field) || Keyword.get(opts, :default))
    builder = Keyword.get(opts, :builder) || &date_builder(&1, opts)
    builder.(datetime_builder(form, field, date_value(value), nil, opts))
  end

  defp date_builder(b, _opts) do
    html_escape [b.(:year, []), raw(" / "), b.(:month, []), raw(" / "), b.(:day, [])]
  end

  defp date_value(%{"year" => year, "month" => month, "day" => day}),
    do: %{year: year, month: month, day: day}
  defp date_value(%{year: year, month: month, day: day}),
    do: %{year: year, month: month, day: day}

  defp date_value({{year, month, day}, _}),
    do: %{year: year, month: month, day: day}
  defp date_value({year, month, day}),
    do: %{year: year, month: month, day: day}

  defp date_value(nil),
    do: %{year: nil, month: nil, day: nil}
  defp date_value(other),
    do: raise(ArgumentError, "unrecognized date #{inspect other}")

  @doc """
  Generates select tags for time.

  Check `datetime_select/3` for more information on options and supported values.
  """
  def time_select(form, field, opts \\ []) do
    value   = Keyword.get(opts, :value, input_value(form, field) || Keyword.get(opts, :default))
    builder = Keyword.get(opts, :builder) || &time_builder(&1, opts)
    builder.(datetime_builder(form, field, nil, time_value(value), opts))
  end

  defp time_builder(b, opts) do
    time = html_escape [b.(:hour, []), raw(" : "), b.(:minute, [])]

    if Keyword.get(opts, :second) do
      html_escape [time, raw(" : "), b.(:second, [])]
    else
      time
    end
  end

  defp time_value(%{"hour" => hour, "minute" => min} = map),
    do: %{hour: hour, minute: min, second: Map.get(map, "second", 0)}
  defp time_value(%{hour: hour, minute: min} = map),
    do: %{hour: hour, minute: min, second: Map.get(map, :second, 0)}

  # Backwards compatibility with Ecto v1.1 versions
  defp time_value(%{"hour" => hour, "min" => min} = map),
    do: %{hour: hour, minute: min, second: Map.get(map, "sec", 0)}
  defp time_value(%{hour: hour, min: min} = map),
    do: %{hour: hour, minute: min, second: Map.get(map, :sec, 0)}

  defp time_value({_, {hour, min, sec, _msec}}),
    do: %{hour: hour, minute: min, second: sec}
  defp time_value({hour, min, sec, _mseg}),
    do: %{hour: hour, minute: min, second: sec}
  defp time_value({_, {hour, min, sec}}),
    do: %{hour: hour, minute: min, second: sec}
  defp time_value({hour, min, sec}),
    do: %{hour: hour, minute: min, second: sec}

  defp time_value(nil),
    do: %{hour: nil, minute: nil, second: nil}
  defp time_value(other),
    do: raise(ArgumentError, "unrecognized time #{inspect other}")

  @months [
    {"January", "1"},
    {"February", "2"},
    {"March", "3"},
    {"April", "4"},
    {"May", "5"},
    {"June", "6"},
    {"July", "7"},
    {"August", "8"},
    {"September", "9"},
    {"October", "10"},
    {"November", "11"},
    {"December", "12"},
  ]

  map = &Enum.map(&1, fn i ->
    i = Integer.to_string(i)
    {String.rjust(i, 2, ?0), i}
  end)

  @days   map.(1..31)
  @hours  map.(0..23)
  @minsec map.(0..59)

  defp datetime_builder(form, field, date, time, parent) do
    id   = Keyword.get(parent, :id, input_id(form, field))
    name = Keyword.get(parent, :name, input_name(form, field))

    fn
      :year, opts when date != nil ->
        {year, _, _}  = :erlang.date()
        {value, opts} = datetime_options(:year, year-5..year+5, id, name, parent, date, opts)
        select(:datetime, :year, value, opts)
      :month, opts when date != nil ->
        {value, opts} = datetime_options(:month, @months, id, name, parent, date, opts)
        select(:datetime, :month, value, opts)
      :day, opts when date != nil ->
        {value, opts} = datetime_options(:day, @days, id, name, parent, date, opts)
        select(:datetime, :day, value, opts)
      :hour, opts when time != nil ->
        {value, opts} = datetime_options(:hour, @hours, id, name, parent, time, opts)
        select(:datetime, :hour, value, opts)
      :minute, opts when time != nil ->
        {value, opts} = datetime_options(:minute, @minsec, id, name, parent, time, opts)
        select(:datetime, :minute, value, opts)
      :second, opts when time != nil ->
        {value, opts} = datetime_options(:second, @minsec, id, name, parent, time, opts)
        select(:datetime, :second, value, opts)
    end
  end

  defp datetime_options(type, values, id, name, parent, datetime, opts) do
    opts = Keyword.merge Keyword.get(parent, type, []), opts
    suff = Atom.to_string(type)

    {value, opts} = Keyword.pop(opts, :options, values)

    {value,
      opts
      |> Keyword.put_new(:id, id <> "_" <> suff)
      |> Keyword.put_new(:name, name <> "[" <> suff <> "]")
      |> Keyword.put_new(:value, Map.get(datetime, type))}
  end

  @doc """
  Generates a label tag.

  The form should either be a `Phoenix.HTML.Form` emitted
  by `form_for` or an atom.

  All given options are forwarded to the underlying tag.
  A default value is provided for `for` attribute but can
  be overriden if you pass a value to the `for` option.
  Text content would be inferred from `field` if not specified.

  ## Examples

      # Assuming form contains a User schema
      label(form, :name, "Name")
      #=> <label for="user_name">Name</label>

      label(:user, :email, "Email")
      #=> <label for="user_email">Email</label>

      label(:user, :email)
      #=> <label for="user_email">Email</label>

      label(:user, :email, class: "control-label")
      #=> <label for="user_email" class="control-label">Email</label>

      label :user, :email do
        "E-mail Address"
      end
      #=> <label for="user_email">E-mail Address</label>

      label :user, :email, class: "control-label" do
        "E-mail Address"
      end
      #=> <label class="control-label" for="user_email">E-mail Address</label>
  """
  def label(form, field) do
    label(form, field, humanize(field), [])
  end

  @doc """
  See `label/2`.
  """
  def label(form, field, text) when is_binary(text) do
    label(form, field, text, [])
  end
  def label(form, field, [do: block]) do
    label(form, field, [], do: block)
  end
  def label(form, field, opts) when is_list(opts) do
    label(form, field, humanize(field), opts)
  end

  @doc """
  See `label/2`.
  """
  def label(form, field, text, opts) when is_binary(text) and is_list(opts) do
    opts = Keyword.put_new(opts, :for, input_id(form, field))
    content_tag(:label, text, opts)
  end
  def label(form, field, opts, [do: block]) do
    opts = Keyword.put_new(opts, :for, input_id(form, field))
    content_tag(:label, opts, do: block)
  end

  # TODO: Remove me on 3.0

  @doc false
  def field_value(form, field, default \\ nil), do: input_value(form, field) || default

  @doc false
  def field_name(form, field), do: input_name(form, field)

  @doc false
  def field_id(form, field), do: input_id(form, field)
end
