defmodule Decimal do
  @moduledoc """
  Decimal arithmetic on arbitrary precision floating-point numbers.

  A number is represented by a signed coefficient and exponent such that: `sign
  * coefficient * 10^exponent`. All numbers are represented and calculated
  exactly, but the result of an operation may be rounded depending on the
  context the operation is performed with, see: `Decimal.Context`. Trailing
  zeros in the coefficient are never truncated to preserve the number of
  significant digits unless explicitly done so.

  There are also special values such as NaN and (+-)Infinity. -0 and +0 are two
  distinct values. Some operations results are not defined and will return NaN.
  This kind of NaN is quiet, any operation returning a number will return
  NaN when given a quiet NaN (the NaN value will flow through all operations).
  The other kind of NaN is signalling which is the value that can be reached
  in `Error.result/1` when the result is NaN. Any operation given a signalling
  NaN return will signal `:invalid_operation`.

  Exceptional conditions are grouped into signals, each signal has a flag and a
  trap enabler in the context. Whenever a signal is triggered it's flag is set
  in the context and will be set until explicitly cleared. If the signal is trap
  enabled `Decimal.Error` will be raised.

  ## Specifications

  * [IBM's General Decimal Arithmetic Specification](http://speleotrove.com/decimal/decarith.html)
  * [IEEE standard 854-1987](http://754r.ucbtest.org/standards/854.pdf)

  This implementation follows the above standards as closely as possible. But at
  some places the implementation diverges from the specification. The reasons
  are different for each case but may be that the specification doesn't map to
  this environment, ease of implementation or that API will be nicer. Still, the
  implementation is close enough that the specifications can be seen as
  additional documentation that can be used when things are unclear.

  The specification models the sign of the number as 1, for a negative number,
  and 0 for a positive number. Internally this implementation models the sign as
  1 or -1 such that the complete number will be: `sign * coefficient *
  10^exponent` and will refer to the sign in documentation as either *positive*
  or *negative*.

  There is currently no maximum or minimum values for the exponent. Because of
  that all numbers are "normal". This means that when an operation should,
  according to the specification, return a number that "underflow" 0 is returned
  instead of Etiny. This may happen when dividing a number with infinity.
  Additionally, overflow, underflow and clamped may never be signalled.
  """

  import Bitwise
  import Kernel, except: [abs: 1, div: 2, max: 2, min: 2, rem: 1, round: 1]

  @power_of_2_to_52 4503599627370496

  @typedoc """
  The coefficient of the power of `10`.  Non-negative because the sign is stored separately in `sign`.

    * `non_neg_integer` - when the `t` represents a number, instead of one of the special values below.
    * `:qNaN` - a quiet NaN was produced by a previous operation.  Quiet NaNs propagate quietly, unlike signaling NaNs
       that return errors (based on the `Decimal.Context`).
    * `:sNaN` - signalling NaN that indicated an error occured that should stop the next operation with an error
       (based on the `Decimal.Context`).
    * `:inf` - infinity
  """
  @type coefficient :: non_neg_integer | :qNaN | :sNaN | :inf

  @typedoc """
  The exponent to which `10` is raised.
  """
  @type exponent :: integer

  @typedoc """
    * `1` for positive
    * `-1` for negative
  """
  @type sign :: 1 | -1

  @type signal :: :invalid_operation |
                  :division_by_zero |
                  :rounded |
                  :inexact

  @type rounding :: :down |
                    :half_up |
                    :half_even |
                    :ceiling |
                    :floor |
                    :half_down |
                    :up

  @typedoc """
  This implementation models the `sign` as `1` or `-1` such that the complete number will be: `sign * coef * 10^exp`.

    * `coef` - The coefficient of the power of `10`.
    * `exp` - The exponent of the power of `10`.
    * `sign` - `1` for positive. `-1` for negative.
  """
  @type t :: %__MODULE__{
    sign: sign,
    coef: coefficient,
    exp: exponent
  }

  defstruct [sign: 1, coef: 0, exp: 0]

  @context_key :"$decimal_context"

  defmodule Error do
    @moduledoc """
    The exception that all Decimal operations may raise.

    ## Fields

    * `signal` - The signalled error, additional signalled errors will be found
      in the context.
    * `reason` - The reason for the error.
    * `result` - The result of the operation signalling the error.

    Rescuing the error to access the result or the other fields of the error is
    discouraged and should only be done for exceptional conditions. It is more
    pragmatic to set the appropriate traps on the context and check the flags
    after the operation if the result needs to be inspected.
    """

    defexception [:message, :signal, :reason, :result]

    def exception(opts) do
      reason = if opts[:reason], do: ": " <> opts[:reason]
      msg    = "#{opts[:signal]}#{reason}"

      struct(__MODULE__, [message: msg] ++ opts)
    end
  end

  defmodule Context do
    @moduledoc """
    The context is kept in the process dictionary. It can be accessed with
    `Decimal.get_context/0` and `Decimal.set_context/1`.

    The default context has a precision of 9, the rounding algorithm is
    `:half_up`. The set trap enablers are `:invalid_operation` and
    `:division_by_zero`.

    ## Fields

    * `precision` - Maximum number of decimal digits in the coefficient. If an
      operation's result has more digits it will be rounded to `precision`
      digits with the rounding algorithm in `rounding`.
    * `rounding` - The rounding algorithm used when the coefficient's number of
      exceeds `precision`. Strategies explained below.
    * `flags` - A list of signals that for which the flag is sent. When an
      exceptional condition is signalled it's flag is set. The flags are sticky
      and will be set until explicitly cleared.
    * `traps` - A list of set trap enablers for signals. When a signal's trap
      enabler is set the condition causes `Decimal.Error` to be raised.

    ## Rounding algorithms

    * `:down` - Round toward zero (truncate). Discarded digits are ignored,
      result is unchanged.
    * `:half_up` - If the discarded digits is greater than or equal to half of
      the value of a one in the next left position then the coefficient will be
      incremented by one (rounded up). Otherwise (the discarded digits are less
      than half) the discarded digits will be ignored.
    * `:half_even` - Also known as "round to nearest" or "banker's rounding". If
      the discarded digits is greater than half of the value of a one in the
      next left position then the coefficient will be incremented by one
      (rounded up). If they represent less than half discarded digits will be
      ignored. Otherwise (exactly half), the coefficient is not altered if it's
      even, or incremented by one (rounded up) if it's odd (to make an even
      number).
    * `:ceiling` - Round toward +Infinity. If all of the discarded digits are
      zero or the sign is negative the result is unchanged. Otherwise, the
      coefficient will be incremented by one (rounded up).
    * `:floor` - Round toward -Infinity. If all of the discarded digits are zero
      or the sign is positive the result is unchanged. Otherwise, the sign is
      negative and coefficient will be incremented by one.
    * `:half_down` - If the discarded digits is greater than half of the value
      of a one in the next left position then the coefficient will be
      incremented by one (rounded up). Otherwise (the discarded digits are half
      or less) the discarded digits are ignored.
    * `:up` - Round away from zero. If all discarded digits are zero the
      coefficient is not changed, otherwise it is incremented by one (rounded
      up).
    """
    @type t :: %__MODULE__{
      precision: pos_integer,
      rounding: Decimal.rounding,
      flags: [Decimal.signal],
      traps: [Decimal.signal]}

    defstruct [
      precision: 28,
      rounding: :half_up,
      flags: [],
      traps: [:invalid_operation, :division_by_zero]]
  end

  defmacrop error(flags, reason, result, context \\ nil) do
    quote bind_quoted: binding() do
      case handle_error(flags, reason, result, context) do
        {:ok, result} -> result
        {:error, error} -> raise Error, error
      end
    end
  end

  @doc """
  Returns `true` if number is NaN; otherwise `false`.
  """
  @spec nan?(t) :: boolean
  def nan?(%Decimal{coef: :sNaN}), do: true
  def nan?(%Decimal{coef: :qNaN}), do: true
  def nan?(%Decimal{}),            do: false

  @doc """
  Returns `true` if number is (+-)Infinity; otherwise `false`.
  """
  @spec inf?(t) :: boolean
  def inf?(%Decimal{coef: :inf}), do: true
  def inf?(%Decimal{}),           do: false

  @doc """
  Returns `true` if argument is a decimal number; otherwise `false`.
  """
  @spec decimal?(any) :: boolean
  def decimal?(%Decimal{}), do: true
  def decimal?(_),          do: false

  @doc """
  The absolute value of given number. Sets the number's sign to positive.
  """
  @spec abs(t) :: t
  def abs(%Decimal{coef: :sNaN} = num) do
    error(:invalid_operation, "operation on NaN", num)
  end

  def abs(%Decimal{coef: :qNaN} = num) do
    %{num | sign: 1}
  end

  def abs(%Decimal{} = num) do
    %{num | sign: 1} |> context
  end

  @doc """
  Adds two numbers together.

  ## Exceptional conditions
  * If one number is -Infinity and the other +Infinity `:invalid_operation` will
  be signalled.
  """
  @spec add(t, t) :: t
  def add(%Decimal{coef: :sNaN} = num1, %Decimal{}) do
    error(:invalid_operation, "operation on NaN", num1)
  end

  def add(%Decimal{}, %Decimal{coef: :sNaN} = num2) do
    error(:invalid_operation, "operation on NaN", num2)
  end

  def add(%Decimal{coef: :qNaN} = num1, %Decimal{}) do
    num1
  end

  def add(%Decimal{}, %Decimal{coef: :qNaN} = num2) do
    num2
  end

  def add(%Decimal{coef: :inf}, %Decimal{coef: :inf}) do
    error(:invalid_operation, "(+-)Infinity + (+-)Infinity", %Decimal{coef: :NaN})
  end

  def add(%Decimal{coef: :inf} = num1, %Decimal{}) do
    num1
  end

  def add(%Decimal{}, %Decimal{coef: :inf} = num2) do
    num2
  end

  def add(%Decimal{sign: sign1, coef: coef1, exp: exp1}, %Decimal{sign: sign2, coef: coef2, exp: exp2}) do
    {coef1, coef2} = add_align(coef1, exp1, coef2, exp2)
    coef = sign1 * coef1 + sign2 * coef2
    exp = Kernel.min(exp1, exp2)
    sign = add_sign(sign1, sign2, coef)
    %Decimal{sign: sign, coef: Kernel.abs(coef), exp: exp} |> context
  end

  @doc """
  Subtracts second number from the first. Equivalent to `Decimal.add/2` when the
  second number's sign is negated.

  ## Exceptional conditions
  * If one number is -Infinity and the other +Infinity `:invalid_operation` will
  be signalled.
  """
  @spec sub(t, t) :: t
  def sub(%Decimal{} = num1, %Decimal{sign: sign} = num2) do
    add(num1, %{num2 | sign: -sign})
  end

  @doc """
  Compares two numbers numerically. If the first number is greater than the second
  `#Decimal<1>` is returned, if less than `Decimal<-1>` is returned. Otherwise,
  if both numbers are equal `Decimal<0>` is returned.  If either number is a quiet
  NaN, then that number is returned.
  """
  @spec compare(t, t) :: t
  def compare(%Decimal{coef: coef1} = num1, %Decimal{coef: coef2} = num2) do
    cond do
      coef1 == :qNaN ->
        num1
      coef2 == :qNaN ->
        num2
      true ->
        case sub(num1, num2) do
          %Decimal{coef: 0} -> %Decimal{sign: 1, coef: 0}
          %Decimal{sign: sign} -> %Decimal{sign: sign, coef: 1}
        end
    end
  end

  @doc """
  Compares two numbers numerically. If the first number is greater than the second
  `:gt` is returned, if less than `:lt` is returned, if both numbers are equal
  `:eq` is returned.

  Neither number can be a `NaN`.  If you need to handle quiet NaNs, use `compare/2`.
  """
  @spec cmp(t, t) :: :lt | :eq | :gt
  def cmp(num1, num2) do
    case compare(num1, num2) do
      %Decimal{coef: 1, sign: -1} -> :lt
      %Decimal{coef: 0} -> :eq
      %Decimal{coef: 1, sign: 1} -> :gt
    end
  end

  @doc """
  Compares two numbers numerically and returns `true` if they are equal,
  otherwise `false`.
  """
  @spec equal?(t, t) :: boolean
  def equal?(num1, num2) do
    case compare(num1, num2) do
      %Decimal{sign: 1, coef: 0, exp: 0} -> true
      _ -> false
    end
  end

  @doc """
  Divides two numbers.

  ## Exceptional conditions
  * If both numbers are (+-)Infinity `:invalid_operation` is signalled.
  * If both numbers are (+-)0 `:invalid_operation` is signalled.
  * If second number (denominator) is (+-)0 `:division_by_zero` is signalled.
  """
  @spec div(t, t) :: t
  def div(%Decimal{coef: :sNaN} = num1, %Decimal{}) do
    error(:invalid_operation, "operation on NaN", num1)
  end

  def div(%Decimal{}, %Decimal{coef: :sNaN} = num2) do
    error(:invalid_operation, "operation on NaN", num2)
  end

  def div(%Decimal{coef: :qNaN} = num1, %Decimal{}) do
    num1
  end

  def div(%Decimal{}, %Decimal{coef: :qNaN} = num2) do
    num2
  end

  def div(%Decimal{coef: :inf}, %Decimal{coef: :inf}) do
    error(:invalid_operation, "(+-)Infinity / (+-)Infinity", %Decimal{coef: :NaN})
  end

  def div(%Decimal{sign: sign1, coef: :inf} = num1, %Decimal{sign: sign2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    %{num1 | sign: sign}
  end

  def div(%Decimal{sign: sign1, exp: exp1}, %Decimal{sign: sign2, coef: :inf, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1

    # TODO: Subnormal
    # exponent?
    %Decimal{sign: sign, coef: 0, exp: exp1 - exp2}
  end

  def div(%Decimal{coef: 0}, %Decimal{coef: 0}) do
    error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})
  end

  def div(%Decimal{sign: sign1}, %Decimal{sign: sign2, coef: 0}) do
    sign = if sign1 == sign2, do: 1, else: -1
    error(:division_by_zero, nil, %Decimal{sign: sign, coef: :inf})
  end

  def div(%Decimal{sign: sign1, coef: coef1, exp: exp1}, %Decimal{sign: sign2, coef: coef2, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1

    if coef1 == 0 do
      %Decimal{sign: sign, coef: 0, exp: exp1 - exp2}
      |> context([])
    else
      prec10 = pow10(get_context().precision)
      {coef1, coef2, adjust} = div_adjust(coef1, coef2, 0)
      {coef, adjust, _rem, signals} = div_calc(coef1, coef2, 0, adjust, prec10)

      %Decimal{sign: sign, coef: coef, exp: exp1 - exp2 - adjust}
      |> context(signals)
    end

  end

  @doc """
  Divides two numbers and returns the integer part.

  ## Exceptional conditions
  * If both numbers are (+-)Infinity `:invalid_operation` is signalled.
  * If both numbers are (+-)0 `:invalid_operation` is signalled.
  * If second number (denominator) is (+-)0 `:division_by_zero` is signalled.
  """
  @spec div_int(t, t) :: t
  def div_int(%Decimal{coef: :sNaN} = num1, %Decimal{}) do
    error(:invalid_operation, "operation on NaN", num1)
  end

  def div_int(%Decimal{}, %Decimal{coef: :sNaN} = num2) do
    error(:invalid_operation, "operation on NaN", num2)
  end

  def div_int(%Decimal{coef: :qNaN} = num1, %Decimal{}) do
    num1
  end

  def div_int(%Decimal{}, %Decimal{coef: :qNaN} = num2) do
    num2
  end

  def div_int(%Decimal{coef: :inf}, %Decimal{coef: :inf}) do
    error(:invalid_operation, "(+-)Infinity / (+-)Infinity", %Decimal{coef: :NaN})
  end

  def div_int(%Decimal{sign: sign1, coef: :inf} = num1, %Decimal{sign: sign2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    %{num1 | sign: sign}
  end

  def div_int(%Decimal{sign: sign1, exp: exp1}, %Decimal{sign: sign2, coef: :inf, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    # TODO: Subnormal
    # exponent?
    %Decimal{sign: sign, coef: 0, exp: exp1 - exp2}
  end

  def div_int(%Decimal{coef: 0}, %Decimal{coef: 0}) do
    error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})
  end

  def div_int(%Decimal{sign: sign1}, %Decimal{sign: sign2, coef: 0}) do
    div_sign = if sign1 == sign2, do: 1, else: -1
    error(:division_by_zero, nil, %Decimal{sign: div_sign, coef: :inf})
  end

  def div_int(%Decimal{sign: sign1, coef: coef1, exp: exp1} = num1, %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2) do
    div_sign = if sign1 == sign2, do: 1, else: -1

    cond do
      compare(%{num1 | sign: 1}, %{num2 | sign: 1}) == %Decimal{sign: -1, coef: 1} ->
        %Decimal{sign: div_sign, coef: 0, exp: exp1 - exp2}
      coef1 == 0 ->
        context(%{num1 | sign: div_sign})
      true ->
        case integer_division(div_sign, coef1, exp1, coef2, exp2) do
          {:ok, result} ->
            result
          {:error, error, reason, num} ->
            error(error, reason, num)
        end
    end
  end

  @doc """
  Remainder of integer division of two numbers. The result will have the sign of
  the first number.

  ## Exceptional conditions
  * If both numbers are (+-)Infinity `:invalid_operation` is signalled.
  * If both numbers are (+-)0 `:invalid_operation` is signalled.
  * If second number (denominator) is (+-)0 `:division_by_zero` is signalled.
  """
  @spec rem(t, t) :: t
  def rem(%Decimal{coef: :sNaN} = num1, %Decimal{}) do
    error(:invalid_operation, "operation on NaN", num1)
  end

  def rem(%Decimal{}, %Decimal{coef: :sNaN} = num2) do
    error(:invalid_operation, "operation on NaN", num2)
  end

  def rem(%Decimal{coef: :qNaN} = num1, %Decimal{}) do
    num1
  end

  def rem(%Decimal{}, %Decimal{coef: :qNaN} = num2) do
    num2
  end

  def rem(%Decimal{coef: :inf}, %Decimal{coef: :inf}) do
    error(:invalid_operation, "(+-)Infinity / (+-)Infinity", %Decimal{coef: :NaN})
  end

  def rem(%Decimal{sign: sign1, coef: :inf}, %Decimal{}) do
    %Decimal{sign: sign1, coef: 0}
  end

  def rem(%Decimal{sign: sign1}, %Decimal{coef: :inf} = num2) do
    # TODO: Subnormal
    # exponent?
    %{num2 | sign: sign1}
  end

  def rem(%Decimal{coef: 0}, %Decimal{coef: 0}) do
    error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})
  end

  def rem(%Decimal{sign: sign1}, %Decimal{coef: 0}) do
    error(:division_by_zero, nil, %Decimal{sign: sign1, coef: 0})
  end

  def rem(%Decimal{sign: sign1, coef: coef1, exp: exp1} = num1, %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2) do
    cond do
      compare(%{num1 | sign: 1}, %{num2 | sign: 1}) == %Decimal{sign: -1, coef: 1} ->
        %{num1 | sign: sign1}
      coef1 == 0 ->
        context(%{num2 | sign: sign1})
      true ->
        div_sign = if sign1 == sign2, do: 1, else: -1
        case integer_division(div_sign, coef1, exp1, coef2, exp2) do
          {:ok, result} ->
            sub(num1, mult(num2, result))
          {:error, error, reason, num} ->
            error(error, reason, num)
        end
    end
  end

  @doc """
  Integer division of two numbers and the remainder. Should be used when both
  `div_int/2` and `rem/2` is needed. Equivalent to: `{Decimal.div_int(x, y),
  Decimal.rem(x, y)}`.

  ## Exceptional conditions
  * If both numbers are (+-)Infinity `:invalid_operation` is signalled.
  * If both numbers are (+-)0 `:invalid_operation` is signalled.
  * If second number (denominator) is (+-)0 `:division_by_zero` is signalled.
  """
  @spec div_rem(t, t) :: {t, t}
  def div_rem(%Decimal{coef: :sNaN} = num1, %Decimal{}) do
    {error(:invalid_operation, "operation on NaN", num1),
      error(:invalid_operation, "operation on NaN", num1)}
  end

  def div_rem(%Decimal{}, %Decimal{coef: :sNaN} = num2) do
    {error(:invalid_operation, "operation on NaN", num2),
      error(:invalid_operation, "operation on NaN", num2)}
  end

  def div_rem(%Decimal{coef: :qNaN} = num1, %Decimal{}) do
    {num1, num1}
  end

  def div_rem(%Decimal{}, %Decimal{coef: :qNaN} = num2) do
    {num2, num2}
  end

  def div_rem(%Decimal{coef: :inf}, %Decimal{coef: :inf}) do
    error(:invalid_operation, "(+-)Infinity / (+-)Infinity", {%Decimal{coef: :NaN}, %Decimal{coef: :NaN}})
  end

  def div_rem(%Decimal{sign: sign1, coef: :inf} = num1, %Decimal{sign: sign2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    {%{num1 | sign: sign}, %Decimal{sign: sign1, coef: 0}}
  end

  def div_rem(%Decimal{sign: sign1, exp: exp1}, %Decimal{sign: sign2, coef: :inf, exp: exp2} = num2) do
    sign = if sign1 == sign2, do: 1, else: -1
    # TODO: Subnormal
    # exponent?
    {%Decimal{sign: sign, coef: 0, exp: exp1 - exp2}, %{num2 | sign: sign1}}
  end

  def div_rem(%Decimal{coef: 0}, %Decimal{coef: 0}) do
    {error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN}),
      error(:invalid_operation, "0 / 0", %Decimal{coef: :NaN})}
  end

  def div_rem(%Decimal{sign: sign1}, %Decimal{sign: sign2, coef: 0}) do
    div_sign = if sign1 == sign2, do: 1, else: -1
    {error(:division_by_zero, nil, %Decimal{sign: div_sign, coef: :inf}),
      error(:division_by_zero, nil, %Decimal{sign: sign1, coef: 0})}
  end

  def div_rem(%Decimal{sign: sign1, coef: coef1, exp: exp1} = num1, %Decimal{sign: sign2, coef: coef2, exp: exp2} = num2) do
    div_sign = if sign1 == sign2, do: 1, else: -1

    cond do
      compare(%{num1 | sign: 1}, %{num2 | sign: 1}) == %Decimal{sign: -1, coef: 1} ->
        {%Decimal{sign: div_sign, coef: 0, exp: exp1 - exp2}, %{num1 | sign: sign1}}
      coef1 == 0 ->
        {context(%{num1 | sign: div_sign}), context(%{num2 | sign: sign1})}
      true ->
        case integer_division(div_sign, coef1, exp1, coef2, exp2) do
          {:ok, result} ->
            {result, sub(num1, mult(num2, result))}
          {:error, error, reason, num} ->
            error(error, reason, {num, num})
        end
    end
  end

  @doc """
  Compares two values numerically and returns the maximum. Unlike most other
  functions in `Decimal` if a number is NaN the result will be the other number.
  Only if both numbers are NaN will NaN be returned.
  """
  @spec max(t, t) :: t
  def max(%Decimal{coef: :qNaN}, %Decimal{} = num2) do
    num2
  end

  def max(%Decimal{} = num1, %Decimal{coef: :qNaN}) do
    num1
  end

  def max(%Decimal{sign: sign1, exp: exp1} = num1, %Decimal{sign: sign2, exp: exp2} = num2) do
    case compare(num1, num2) do
      %Decimal{sign: -1, coef: 1} ->
        num2
      %Decimal{sign: 1, coef: 1} ->
        num1
      %Decimal{coef: 0} ->
        cond do
          sign1 != sign2 ->
            if sign1 == 1, do: num1, else: num2
          sign1 == 1 ->
            if exp1 > exp2, do: num1, else: num2
          sign1 == -1 ->
            if exp1 < exp2, do: num1, else: num2
        end
    end |> context
  end

  @doc """
  Compares two values numerically and returns the minimum. Unlike most other
  functions in `Decimal` if a number is NaN the result will be the other number.
  Only if both numbers are NaN will NaN be returned.
  """
  @spec min(t, t) :: t
  def min(%Decimal{coef: :qNaN}, %Decimal{} = num2) do
    num2
  end

  def min(%Decimal{} = num1, %Decimal{coef: :qNaN}) do
    num1
  end

  def min(%Decimal{sign: sign1, exp: exp1} = num1, %Decimal{sign: sign2, exp: exp2} = num2) do
    case compare(num1, num2) do
      %Decimal{sign: -1, coef: 1} ->
        num1
      %Decimal{sign: 1, coef: 1} ->
        num2
      %Decimal{coef: 0} ->
        cond do
          sign1 != sign2 ->
            if sign1 == -1, do: num1, else: num2
          sign1 == 1 ->
            if exp1 < exp2, do: num1, else: num2
          sign1 == -1 ->
            if exp1 > exp2, do: num1, else: num2
        end
    end |> context
  end

  @doc """
  Negates the given number.
  """
  @spec minus(t) :: t
  def minus(%Decimal{coef: :sNaN} = num) do
    error(:invalid_operation, "operation on NaN", num)
  end

  def minus(%Decimal{coef: :qNaN} = num) do
    num
  end

  def minus(%Decimal{sign: sign} = num) do
    %{num | sign: -sign} |> context
  end

  @doc """
  Applies the context to the given number rounding it to specified precision.
  """
  @spec plus(t) :: t
  def plus(%Decimal{coef: :sNaN} = num) do
    error(:invalid_operation, "operation on NaN", num)
  end

  def plus(%Decimal{} = num) do
    context(num)
  end

  @doc """
  Multiplies two numbers.

  ## Exceptional conditions
  * If one number is (+-0) and the other is (+-)Infinity `:invalid_operation` is
    signalled.
  """
  @spec mult(t, t) :: t
  def mult(%Decimal{coef: :sNaN} = num1, %Decimal{}) do
    error(:invalid_operation, "operation on NaN", num1)
  end

  def mult(%Decimal{}, %Decimal{coef: :sNaN} = num2) do
    error(:invalid_operation, "operation on NaN", num2)
  end

  def mult(%Decimal{coef: :qNaN} = num1, %Decimal{}) do
    num1
  end

  def mult(%Decimal{}, %Decimal{coef: :qNaN} = num2) do
    num2
  end

  def mult(%Decimal{coef: 0}, %Decimal{coef: :inf}) do
    error(:invalid_operation, "0 * (+-)Infinity", %Decimal{coef: :NaN})
  end

  def mult(%Decimal{coef: :inf}, %Decimal{coef: 0}) do
    error(:invalid_operation, "0 * (+-)Infinity", %Decimal{coef: :NaN})
  end

  def mult(%Decimal{sign: sign1, coef: :inf, exp: exp1}, %Decimal{sign: sign2, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    # exponent?
    %Decimal{sign: sign, coef: :inf, exp: exp1 + exp2}
  end

  def mult(%Decimal{sign: sign1, exp: exp1}, %Decimal{sign: sign2, coef: :inf, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    # exponent?
    %Decimal{sign: sign, coef: :inf, exp: exp1 + exp2}
  end

  def mult(%Decimal{sign: sign1, coef: coef1, exp: exp1}, %Decimal{sign: sign2, coef: coef2, exp: exp2}) do
    sign = if sign1 == sign2, do: 1, else: -1
    %Decimal{sign: sign, coef: coef1 * coef2, exp: exp1 + exp2} |> context
  end

  @doc """
  Reduces the given number. Removes trailing zeros from coefficient while
  keeping the number numerically equivalent by increasing the exponent.
  """
  @spec reduce(t) :: t
  def reduce(%Decimal{coef: :sNaN} = num) do
    error(:invalid_operation, "operation on NaN", num)
  end

  def reduce(%Decimal{coef: :qNaN} = num) do
    num
  end

  def reduce(%Decimal{coef: :inf} = num) do
    # exponent?
    %{num | exp: 0}
  end

  def reduce(%Decimal{sign: sign, coef: coef, exp: exp}) do
    if coef == 0 do
      %Decimal{sign: sign, coef: 0, exp: 0}
    else
      %{do_reduce(coef, exp) | sign: sign} |> context
    end
  end

  @doc """
  Rounds the given number to specified decimal places with the given strategy
  (default is to round to nearest one). If places is negative, at least that
  many digits to the left of the decimal point will be zero.
  """
  @spec round(t, integer, rounding) :: t
  def round(num, places \\ 0, mode \\ :half_up)

  def round(%Decimal{coef: :sNaN} = num, _, _) do
    error(:invalid_operation, "operation on NaN", num)
  end

  def round(%Decimal{coef: :qNaN} = num, _, _) do
    num
  end

  def round(%Decimal{coef: :inf} = num, _, _) do
    num
  end

  def round(num, n, mode) do
    %Decimal{sign: sign, coef: coef, exp: exp} = reduce(num)
    digits = :erlang.integer_to_list(coef)
    target_exp = -n
    value = do_round(sign, digits, exp, target_exp, mode)
    context(value, [])
  end

  @doc """
  Creates a new decimal number from a string representation, an integer or a
  floating point number. Floating point numbers will be converted to decimal
  numbers with `:io_lib_format.fwrite_g/1`, since this conversion is not exact
  it is recommended to give an integer or a string when possible.

  A decimal number will always be created exactly as specified with all digits
  kept - it will not be rounded with the context.

  ## BNFC

      sign           ::=  ’+’ | ’-’
      digit          ::=  ’0’ | ’1’ | ’2’ | ’3’ | ’4’ | ’5’ | ’6’ | ’7’ | ’8’ | ’9’
      indicator      ::=  ’e’ | ’E’
      digits         ::=  digit [digit]...
      decimal-part   ::=  digits ’.’ [digits] | [’.’] digits
      exponent-part  ::=  indicator [sign] digits
      infinity       ::=  ’Infinity’ | ’Inf’
      nan            ::=  ’NaN’ [digits] | ’sNaN’ [digits]
      numeric-value  ::=  decimal-part [exponent-part] | infinity
      numeric-string ::=  [sign] numeric-value | [sign] nan
  """
  @spec new(t | integer | float | String.t) :: t
  def new(%Decimal{} = num),
    do: num
  def new(int) when is_integer(int),
    do: %Decimal{sign: (if int < 0, do: -1, else: 1), coef: Kernel.abs(int)}
  def new(float) when is_float(float),
    do: float |> :io_lib_format.fwrite_g |> IO.iodata_to_binary |> new
  def new(binary) when is_binary(binary) do
    case do_parse(binary) do
      {:ok, decimal} -> decimal
      {:error, error} -> raise Error, error
    end
  end

  @doc """
  Creates a new decimal number from the sign, coefficient and exponent such that
  the number will be: `sign * coefficient * 10^exponent`.

  A decimal number will always be created exactly as specified with all digits
  kept - it will not be rounded with the context.
  """
  @spec new(1 | -1, non_neg_integer | :qNaN | :sNaN | :inf, integer) :: t
  def new(sign, coefficient, exponent) do
    %Decimal{sign: sign, coef: coefficient, exp: exponent}
  end

  @doc """
  Parses a binary into a decimal.

  If successful, returns a tuple in the form of `{:ok, decimal}`.
  Otherwise `:error`.

  ## Examples

      iex> Decimal.parse("3.14")
      {:ok, Decimal.new(3.14)}

      iex> Decimal.parse("-1.1e3")
      {:ok, Decimal.new(-1.1e3)}

      iex> Decimal.parse("bad")
      :error
  """
  @spec parse(String.t) :: {:ok, t} | :error
  def parse(binary) when is_binary(binary) do
    case do_parse(binary) do
      {:ok, decimal} -> {:ok, decimal}
      {:error, _} -> :error
    end
  end

  @doc """
  Converts given number to its string representation.

  ## Options
  * `:scientific` - Number converted to scientific notation.
  * `:normal` - Number converted without a exponent.
  * `:raw` - Number converted to it's raw, internal format.
  """
  @spec to_string(t, :scientific | :normal | :raw) :: String.t
  def to_string(num, type \\ :scientific)

  def to_string(%Decimal{sign: sign, coef: :qNaN}, _type) do
    if sign == 1, do: "NaN", else: "-NaN"
  end

  def to_string(%Decimal{sign: sign, coef: :sNaN}, _type) do
    if sign == 1, do: "sNaN", else: "-sNaN"
  end

  def to_string(%Decimal{sign: sign, coef: :inf}, _type) do
    if sign == 1, do: "Infinity", else: "-Infinity"
  end

  def to_string(%Decimal{sign: sign, coef: coef, exp: exp}, :normal) do
    list = Integer.to_char_list(coef)

    list =
      if exp >= 0 do
        list ++ :lists.duplicate(exp, ?0)
      else
        diff = length(list) + exp
        if diff > 0 do
          List.insert_at(list, diff, ?.)
        else
          '0.' ++ :lists.duplicate(-diff, ?0) ++ list
        end
      end

    list = if sign == -1, do: [?-|list], else: list
    IO.iodata_to_binary(list)
  end

  def to_string(%Decimal{sign: sign, coef: coef, exp: exp}, :scientific) do
    list = Integer.to_char_list(coef)
    length = length(list)
    adjusted = exp + length - 1

    list =
      cond do
        exp == 0 ->
          list

        exp < 0 and adjusted >= -6 ->
          abs_exp = Kernel.abs(exp)
          diff = -length + abs_exp + 1
          if diff > 0 do
            list = :lists.duplicate(diff, ?0) ++ list
            List.insert_at(list, 1, ?.)
          else
            List.insert_at(list, exp - 1, ?.)
          end

        true ->
          list = if length > 1, do: List.insert_at(list, 1, ?.), else: list
          list = list ++ 'E'
          list = if exp >= 0, do: list ++ '+', else: list
          list ++ Integer.to_char_list(adjusted)
      end

    list = if sign == -1, do: [?-|list], else: list
    IO.iodata_to_binary(list)
  end

  def to_string(%Decimal{sign: sign, coef: coef, exp: exp}, :raw) do
    str = Integer.to_string(coef)
    str = if sign == -1, do: [?-|str], else: str
    str = if exp != 0, do: [str, "E", Integer.to_string(exp)], else: str

    IO.iodata_to_binary(str)
  end

  @doc """
  Returns the decimal represented as an integer.

  Fails when loss of precision will occur.
  """
  @spec to_integer(t) :: integer
  def to_integer(%Decimal{sign: sign, coef: coef, exp: 0})
  when is_integer(coef) do
    sign * coef
  end

  def to_integer(%Decimal{sign: sign, coef: coef, exp: exp})
  when is_integer(coef) and exp > 0 do
    to_integer(%Decimal{sign: sign, coef: coef * 10, exp: exp - 1})
  end

  def to_integer(%Decimal{sign: sign, coef: coef, exp: exp})
  when is_integer(coef) and exp < 0 and Kernel.rem(coef, 10) == 0 do
    to_integer(%Decimal{sign: sign, coef: trunc(coef / 10), exp: exp + 1})
  end

  @doc """
  Returns the decimal converted to a float.

  The returned float may have lower precision than the Decimal. Fails if
  the decimal cannot be converted to a float.
  """
  @spec to_float(t) :: float
  def to_float(%Decimal{sign: sign, coef: coef, exp: exp}) when is_integer(coef) do
    # Convert back to float without loss
    # http://www.exploringbinary.com/correct-decimal-to-floating-point-using-big-integers/
    {num, den} = ratio(coef, exp)

    boundary = den <<< 52

    cond do
      num == 0 ->
        0.0
      num >= boundary ->
        {den, exp} = scale_down(num, boundary, 52)
        decimal_to_float(sign, num, den, exp)
      true ->
        {num, exp} = scale_up(num, boundary, 52)
        decimal_to_float(sign, num, den, exp)
    end
  end

  defp scale_up(num, den, exp) when num >= den, do: {num, exp}
  defp scale_up(num, den, exp), do: scale_up(num <<< 1, den, exp - 1)

  defp scale_down(num, den, exp) do
    new_den = den <<< 1
    if num < new_den do
      {den >>> 52, exp}
    else
      scale_down(num, new_den, exp + 1)
    end
  end

  defp decimal_to_float(sign, num, den, exp) do
    quo = Kernel.div(num, den)
    rem = num - quo * den

    tmp =
      case den >>> 1 do
        den when rem > den -> quo + 1
        den when rem < den -> quo
        _ when (quo &&& 1) === 1 -> quo + 1
        _ -> quo
      end

    sign = if sign == -1, do: 1, else: 0
    tmp = tmp - @power_of_2_to_52
    <<tmp::float>> = <<sign::size(1), (exp + 1023)::size(11), tmp::size(52)>>
    tmp
  end

  @doc """
  Runs function with given context.
  """
  @spec with_context(Context.t, (() -> x)) :: x when x: var
  def with_context(%Context{} = context, fun) when is_function(fun, 0) do
    old = Process.put(@context_key, context)
    try do
      fun.()
    after
      set_context(old || %Context{})
    end
  end

  @doc """
  Gets the process' context.
  """
  @spec get_context() :: Context.t
  def get_context do
    Process.get(@context_key, %Context{})
  end

  @doc """
  Set the process' context.
  """
  @spec set_context(Context.t) :: :ok
  def set_context(%Context{} = context) do
    Process.put(@context_key, context)
    :ok
  end

  @doc """
  Update the process' context.
  """
  @spec update_context((Context.t -> Context.t)) :: :ok
  def update_context(fun) when is_function(fun, 1) do
    get_context() |> fun.() |> set_context
  end

  ## ARITHMETIC ##

  defp add_align(coef1, exp1, coef2, exp2) when exp1 == exp2,
    do: {coef1, coef2}

  defp add_align(coef1, exp1, coef2, exp2) when exp1 > exp2,
    do: {coef1 * pow10(exp1 - exp2), coef2}

  defp add_align(coef1, exp1, coef2, exp2) when exp1 < exp2,
    do: {coef1, coef2 * pow10(exp2 - exp1)}

  defp add_sign(sign1, sign2, coef) do
    cond do
      coef > 0 -> 1
      coef < 0 -> -1
      sign1 == -1 and sign2 == -1 -> -1
      sign1 != sign2 and get_context().rounding == :floor -> -1
      true -> 1
    end
  end

  defp div_adjust(coef1, coef2, adjust) when coef1 < coef2,
    do: div_adjust(coef1 * 10, coef2, adjust + 1)

  defp div_adjust(coef1, coef2, adjust) when coef1 >= coef2 * 10,
    do: div_adjust(coef1, coef2 * 10, adjust - 1)

  defp div_adjust(coef1, coef2, adjust),
    do: {coef1, coef2, adjust}

  defp div_calc(coef1, coef2, coef, adjust, prec10) do
    cond do
      coef1 >= coef2 ->
        div_calc(coef1 - coef2, coef2, coef + 1, adjust, prec10)

      coef1 == 0 and adjust >= 0 ->
        {coef, adjust, coef1, []}

      coef >= prec10 ->
        signals = [:rounded]
        signals = if base10?(coef1), do: signals, else: [:inexact|signals]
        {coef, adjust, coef1, signals}

      true ->
        div_calc(coef1 * 10, coef2, coef * 10, adjust + 1, prec10)
    end
  end

  defp div_int_calc(coef1, coef2, coef, adjust, precision) do
    cond do
      coef1 >= coef2 ->
        div_int_calc(coef1 - coef2, coef2, coef + 1, adjust, precision)
      adjust != precision ->
        div_int_calc(coef1 * 10, coef2, coef * 10, adjust + 1, precision)
      true ->
        {coef, coef1}
    end
  end

  defp integer_division(div_sign, coef1, exp1, coef2, exp2) do
    precision = exp1 - exp2
    {coef1, coef2, adjust} = div_adjust(coef1, coef2, 0)

    {coef, _rem} = div_int_calc(coef1, coef2, 0, adjust, precision)

    prec10 = pow10(get_context().precision)

    if coef > prec10 do
      {:error, :invalid_operation, "integer division impossible, quotient too large", %Decimal{coef: :NaN}}
    else
      {:ok, %Decimal{sign: div_sign, coef: coef, exp: 0}}
    end
  end

  defp do_reduce(0, _exp) do
    %Decimal{coef: 0, exp: 0}
  end

  defp do_reduce(coef, exp) do
    if Kernel.rem(coef, 10) == 0 do
      do_reduce(Kernel.div(coef, 10), exp + 1)
    else
      %Decimal{coef: coef, exp: exp}
    end
  end

  defp ratio(coef, exp) when exp >= 0, do: {coef * pow10(exp), 1}
  defp ratio(coef, exp) when exp < 0, do: {coef, pow10(-exp)}

  pow10_max = Enum.reduce 0..104, 1, fn x, acc ->
    defp pow10(unquote(x)), do: unquote(acc)
    defp base10?(unquote(acc)), do: true
    acc * 10
  end

  defp pow10(num) when num > 104, do: pow10(104) * pow10(num-104)

  defp base10?(num) when num > unquote(pow10_max) do
    if Kernel.rem(num, unquote(pow10_max)) == 0 do
      base10?(Kernel.div(num, unquote(pow10_max)))
    else
      false
    end
  end
  defp base10?(_num), do: false

  ## ROUNDING ##

  defp do_round(sign, digits, exp, target_exp, rounding) do
    num_digits = length(digits)
    precision = num_digits - (target_exp - exp)

    cond do
      exp == target_exp ->
        %Decimal{sign: sign, coef: digits_to_integer(digits), exp: exp}

      exp < target_exp and precision < 0 ->
        digits = [?0|digits]
        {signif, remain} = :lists.split(1, digits)
        signif = if increment?(rounding, sign, signif, remain), do: digits_increment(signif), else: signif
        coef = digits_to_integer(signif)
        %Decimal{sign: sign, coef: coef, exp: target_exp}

      exp < target_exp and precision >= 0 ->
        {signif, remain} = :lists.split(precision, digits)
        signif = if increment?(rounding, sign, signif, remain), do: digits_increment(signif), else: signif
        coef = digits_to_integer(signif)
        %Decimal{sign: sign, coef: coef, exp: target_exp}

      exp > target_exp ->
        digits = digits ++ Enum.map(1..(exp-target_exp), fn _ -> ?0 end)
        coef = digits_to_integer(digits)
        %Decimal{sign: sign, coef: coef, exp: target_exp}
    end
  end

  defp digits_to_integer([]), do: 0
  defp digits_to_integer(digits), do: :erlang.list_to_integer(digits)

  defp precision(%Decimal{coef: :sNaN} = num, _precision, _rounding) do
    {num, []}
  end

  defp precision(%Decimal{coef: :qNaN} = num, _precision, _rounding) do
    {num, []}
  end

  defp precision(%Decimal{coef: :inf} = num, _precision, _rounding) do
    {num, []}
  end

  defp precision(%Decimal{sign: sign, coef: coef, exp: exp} = num, precision, rounding) do
    digits = :erlang.integer_to_list(coef)
    num_digits = length(digits)

    if num_digits > precision do
      do_precision(sign, digits, num_digits, exp, precision, rounding)
    else
      {num, []}
    end
  end

  defp do_precision(sign, digits, num_digits, exp, precision, rounding) do
    precision = Kernel.min(num_digits, precision)
    {signif, remain} = :lists.split(precision, digits)

    signif = if increment?(rounding, sign, signif, remain), do: digits_increment(signif), else: signif

    signals = if any_nonzero(remain), do: [:inexact, :rounded], else: [:rounded]

    exp = exp + length(remain)
    coef = digits_to_integer(signif)
    dec = %Decimal{sign: sign, coef: coef, exp: exp}
    {dec, signals}
  end

  defp increment?(_, _, _, []),
    do: false

  defp increment?(:down, _, _, _),
    do: false

  defp increment?(:up, _, _, _),
    do: true

  defp increment?(:ceiling, sign, _, remain),
    do: sign == 1 and any_nonzero(remain)

  defp increment?(:floor, sign, _, remain),
    do: sign == -1 and any_nonzero(remain)

  defp increment?(:half_up, _, _, [digit|_]),
    do: digit >= ?5

  defp increment?(:half_even, _, signif, [?5|rest]),
    do: any_nonzero(rest) or Kernel.rem(:lists.last(signif), 2) == 1

  defp increment?(:half_even, _, _, [digit|_]),
    do: digit > ?5

  defp increment?(:half_down, _, _, [digit|_]),
    do: digit > ?5

  defp any_nonzero(digits),
    do: :lists.any(fn digit -> digit != ?0 end, digits)

  defp digits_increment(digits),
    do: digits_increment(:lists.reverse(digits), [])

  defp digits_increment([?9|rest], acc),
    do: digits_increment(rest, [?0|acc])

  defp digits_increment([head|rest], acc),
    do: :lists.reverse(rest, [head+1|acc])

  defp digits_increment([], acc),
    do: [?1|acc]

  ## CONTEXT ##

  defp context(num, signals \\ []) do
    ctxt = get_context()
    {result, prec_signals} = precision(num, ctxt.precision, ctxt.rounding)
    error(put_uniq(signals, prec_signals), nil, result, ctxt)
  end

  defp put_uniq(list, elems) when is_list(elems) do
    Enum.reduce(elems, list, &put_uniq(&2, &1))
  end

  defp put_uniq(list, elem) do
    if elem in list, do: list, else: [elem|list]
  end

  ## PARSING ##

  defp do_parse("+" <> bin) do
    String.downcase(bin) |> parse_unsign
  end

  defp do_parse("-" <> bin) do
    case String.downcase(bin) |> parse_unsign do
      {:ok, num} -> {:ok, %{num | sign: -1}}
      {:error, error} -> {:error, error}
    end
  end

  defp do_parse(bin) do
    String.downcase(bin) |> parse_unsign
  end

  defp parse_unsign("inf") do
    {:ok, %Decimal{coef: :inf}}
  end

  defp parse_unsign("infinity") do
    {:ok, %Decimal{coef: :inf}}
  end

  defp parse_unsign("snan") do
    {:ok, %Decimal{coef: :sNaN}}
  end

  defp parse_unsign("nan") do
    {:ok, %Decimal{coef: :qNaN}}
  end

  defp parse_unsign(bin) do
    {int, rest} = parse_digits(bin)
    {float, rest} = parse_float(rest)
    {exp, rest} = parse_exp(rest)

    if rest != "" or (int == [] and float == []) do
      handle_error(:invalid_operation, "number parsing syntax", %Decimal{coef: :NaN}, nil)
    else
      int = if int == [], do: '0', else: int
      exp = if exp == [], do: '0', else: exp
      {:ok, %Decimal{coef: List.to_integer(int ++ float), exp: List.to_integer(exp) - length(float)}}
    end
  end

  defp parse_float("." <> rest), do: parse_digits(rest)
  defp parse_float(bin), do: {[], bin}

  defp parse_exp(<< ?e, rest :: binary >>) do
    case rest do
      << sign, rest :: binary >> when sign in [?+, ?-] ->
        {digits, rest} = parse_digits(rest)
        {[sign|digits], rest}
      _ ->
        parse_digits(rest)
    end
  end

  defp parse_exp(bin) do
    {[], bin}
  end

  defp parse_digits(bin), do: parse_digits(bin, [])

  defp parse_digits(<< digit, rest :: binary >>, acc) when digit in ?0..?9 do
    parse_digits(rest, [digit|acc])
  end

  defp parse_digits(rest, acc) do
    {:lists.reverse(acc), rest}
  end

  # Util

  defp handle_error(signals, reason, result, context) do
    context = context || get_context()
    signals = List.wrap(signals)

    flags = Enum.reduce(signals, context.flags, &put_uniq(&2, &1))
    set_context(%{context | flags: flags})

    error_signal = Enum.find(signals, &(&1 in context.traps))
    nan = if error_signal, do: :sNaN, else: :qNaN

    result = if match?(%Decimal{coef: :NaN}, result), do: %{result | coef: nan}, else: result

    if error_signal do
      error = [signals: error_signal, reason: reason, result: result]
      {:error, error}
    else
      {:ok, result}
    end
  end
end

defimpl Inspect, for: Decimal do
  def inspect(dec, _opts) do
    "#Decimal<" <> Decimal.to_string(dec) <> ">"
  end
end

defimpl String.Chars, for: Decimal do
  def to_string(dec) do
    Decimal.to_string(dec)
  end
end
