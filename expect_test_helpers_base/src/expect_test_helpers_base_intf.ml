open! Base

module CR = struct
  type t =
    | CR
    | CR_soon
    | CR_soon_for of string
    | CR_someday
    | Comment
    | Suppress
  [@@deriving sexp_of]
end

module Sexp_style = struct
  type t =
    | To_string_mach
    | To_string_hum
    | Pretty of Sexp_pretty.Config.t
  [@@deriving sexp_of]
end

module type With_compare = sig
  type t : any [@@deriving sexp_of]

  val compare : [%compare: t]
end

module type With_equal = sig
  type t : any [@@deriving sexp_of]

  val equal : t -> t -> bool
end

module type With_round_trip = sig
  type t
  type repr [@@deriving sexp_of]

  val to_repr : t -> repr
  val of_repr : repr -> t
  val repr_name : string
end

module type With_sexpable = sig
  type t [@@deriving equal, sexp]
end

module type With_stringable = sig
  type t [@@deriving equal]

  include Stringable.S with type t := t
end

module type With_quickcheck_and_compare = sig
  type t [@@deriving compare]

  include Base_quickcheck.Test.S with type t := t
end

module type With_quickcheck_and_equal = sig
  type t [@@deriving equal]

  include Base_quickcheck.Test.S with type t := t
end

module type With_quickcheck_and_compare_and_equal = sig
  type t [@@deriving compare, equal]

  include Base_quickcheck.Test.S with type t := t
end

module Quickcheck = Base_quickcheck

module type Expect_test_helpers_base = sig
  (** Helpers for producing output inside [let%expect_test]. Designed for code using
      [Base]. See also [Expect_test_helpers_core] and [Expect_test_helpers_async]. *)

  module type With_compare = With_compare
  module type With_equal = With_equal
  module type With_round_trip = With_round_trip
  module type With_sexpable = With_sexpable
  module type With_stringable = With_stringable
  module type With_quickcheck_and_compare = With_quickcheck_and_compare
  module type With_quickcheck_and_equal = With_quickcheck_and_equal

  module type With_quickcheck_and_compare_and_equal =
    With_quickcheck_and_compare_and_equal

  module CR : sig
    include module type of struct
      include CR
    end

    (** [hide_unstable_output t] returns [false] if [t = CR] and [true] otherwise. Useful
        to provide a default for arguments such as [?hide_positions] in functions that
        also have a [?cr] argument. *)
    val hide_unstable_output : t -> bool
  end

  module Phys_equal (M : sig
      type t [@@deriving sexp_of]
    end) : With_equal with type t = M.t

  module Sexp_style : sig
    include module type of struct
      include Sexp_style
    end

    (** Pretty-printing via [Sexp_pretty] with default config, except no colors. *)
    val default_pretty : t

    (** Pretty-printing via [Sexp_pretty] with most heuristics disabled. *)
    val simple_pretty : t
  end

  module Expectation : sig
    (** Tools for managing [[%expectation]] blocks. *)

    (** All functions in this module raise if called outside an expect test. *)

    (** {2 Expectation lifecycle}

        An expectation becomes "active" when execution of a test encounters an
        [[%expectation]] block. Only one expectation can be active at a time. The active
        expectation remains active until a call to [commit], [skip], or [reset], at which
        point it is resolved and becomes inactive.

        After each [[%expectation]], one of [commit], [skip], or [reset] must be called
        before the next [[%expect]], [[%expect_exact]], [[%expectation]], or the end of
        [let%expect_test]. *)

    (** Returns [true] if there is currently an active expectation, [false] otherwise. *)
    val is_active : here:[%call_pos] -> unit -> bool

    (** Accepts the test result for the currently active expectation.

        If [is_successful ()], [commit ()] does not produce a diff in expect test, just as
        when an [[%expect]] block matches. Otherwise, [commit ()] causes the contents of
        the [[%expectation]] to be replaced with the actual output in the corrected file.

        If there is an [[%expectation]] block for which [commit ()] is never called, it is
        replaced by a [[%expectation.never_committed]] node in the corrected file.

        Raises if no expectation is active (i.e., if [is_active ()] returns [false]). *)
    val commit : here:[%call_pos] -> unit -> unit

    (** Skips the currently active expectation.

        This means that no change is made to the [[%expectation]] block in the corrected
        file, even if [not (is_successful ())].

        A [skip]'d [[%expectation]] still consumes its input. For example:
        {[
          let%expect_test _ =
            print_endline "right";
            [%expectation {| wrong |}];
            Expectation.skip ();
            [%expect {| |}]
          ;;
        ]}
        The [[%expect]] block at the end of the test receives no input.

        Raises if no expectation is active (i.e., if [is_active ()] returns [false]). *)
    val skip : here:[%call_pos] -> unit -> unit

    (** Like [skip ()], but also resets the collected input. So the test from the [skip]
        documentation would instead look like this:
        {[
          let%expect_test _ =
            print_endline "right";
            [%expectation {| wrong |}];
            Expectation.reset ();
            [%expect {| right |}]
          ;;
        ]}

        Raises if no expectation is active (i.e., if [is_active ()] returns [false]). *)
    val reset : here:[%call_pos] -> unit -> unit

    (** {2 Expectation helpers}

        The following functions may be called any number of times while an expectation is
        active (i.e., after encountering an [[%expectation]] and before calling [commit],
        [skip], or [reset]). They raise if called when no expectation is active. *)

    (** Produces the output consumed by the currently active expectation. Raises if no
        expectation is active. *)
    val actual : here:[%call_pos] -> unit -> string

    (** Produces the output expected by the currently active expectation. Is [None] for
        [[%expectation.never_committed]] blocks. Raises if no expectation is active. *)
    val expected : here:[%call_pos] -> unit -> string option

    (** Reports whether [actual ()] matches [expected ()] for the currently active
        expectation. This is not the same as [String.equal (actual ()) (expected ())] due
        to whitespace normalization. Raises if no expectation is active. *)
    val is_successful : here:[%call_pos] -> unit -> bool

    (**/**)

    (** Produces a sexp representing the state of the currently active expectation. Raises
        if no expectation is active. *)
    val sexp_for_debugging : here:[%call_pos] -> unit -> Sexp.t
  end

  (** [hide_positions_in_string] does line-based regexp matching to replace line numbers
      and column numbers that appear in source-code positions with constant text [LINE]
      and [COL]. This can be useful in making displayed test output less fragile. *)
  val hide_positions_in_string : string -> string

  (** [hide_temp_files_in_string] replaces [.tmp.______], where each [_] represents some
      alphanumeric character, with ".tmp.RANDOM". This can make output deterministic when
      describing temporary files generated by, e.g., [Core_unix.mkstemp]. *)
  val hide_temp_files_in_string : string -> string

  (** Renders an s-expression as a string. With [~hide_positions:true], patterns in the
      string that match OCaml-style file positions are modified to hide the line number,
      column number, and character positions, to make output less fragile. *)
  val sexp_to_string : ?hide_positions:bool (** default is [false] *) -> Sexp.t -> string

  (** Substitutes [with_] for every occurrence of [pattern] in a string. *)
  val replace : string -> pattern:string -> with_:string -> string

  (** Like [replace], for every atom in a sexp. *)
  val replace_s : Sexp.t -> pattern:string -> with_:string -> Sexp.t

  (** Applies [f] at every node in the given sexp, top-down, recurring on the contents of
      the output. The word "smash" is used as in the sexp command-line tool's query
      language. See: https://github.com/janestreet/sexp *)
  val smash_sexp : Sexp.t -> f:(Sexp.t -> Sexp.t) -> Sexp.t

  (** Removes OCaml backtraces from sexps. *)
  val remove_backtraces : Sexp.t -> Sexp.t

  (** For printing an s-expression to stdout. [hide_positions] works as in
      [sexp_to_string]. *)
  val print_s : ?hide_positions:bool (** default is [false] *) -> Sexp.t -> unit

  val print_string : ?hide_positions:bool (** default is [false] *) -> string -> unit
  val print_endline : ?hide_positions:bool (** default is [false] *) -> string -> unit

  (** Behaves like [[%expect.output]].

      Raises if called when not running an expect test. *)
  val expect_test_output : here:[%call_pos] -> unit -> string

  (** Sets aside output generated up to now. Within the callback, only new output will be
      captured by [[%expect]], [[%expect.output]], and the like. After the callback, the
      old output is prepended to any remaining new output.

      This can be used for running and modifying a selected portion of test output,
      without having to remove and re-add anything prior. For example, you could run some
      noisy tests inside this, then decide to erase their output if they did not trigger
      [on_print_cr] (below). *)
  val with_empty_expect_test_output : here:[%call_pos] -> local_ (unit -> 'a) -> 'a

  (** Raises an error if, in the current test:
      1. Control flow has reached a [[%expect.unreachable]] node or
      2. Control flow has reached a [[%expect _]/[%expect_exact _]/[%expect.if_reached _]]
         node, and the output collected did not match the output expected at that node.

      An error raised in the above cases begins with [message], if provided.

      Also raises if called when not running an expect test. *)
  val raise_if_output_did_not_match : ?message:string -> here:[%call_pos] -> unit -> unit

  (** Returns [true] if running inside the body of [let%expect_test], or [false]
      otherwise. Use to test whether [expect_test_output] will raise, for example. Unlike
      [Core.am_running_test], this is not configured by an environment variable, and is
      not inherited by child processes. *)
  val am_running_expect_test : unit -> bool

  (** Returns the name of the currently-running expect-test, or [None] if the expect test
      has no name (is defined via [let%expect_test _ = ...]).

      Raises if called when not running an expect test. *)
  val current_expect_test_name_exn : here:[%call_pos] -> unit -> string option

  (** If [am_running_expect_test () = false], raises with an explanatory error message. Do
      not use [require am_running_expect_test], as outside of an expect test that may not
      accomplish anything. *)
  val assert_am_running_expect_test : here:[%call_pos] -> unit -> unit

  (** [print_cr message] prints a [CR require-failed], which will appear in expect-test
      output. The CR will appear in the feature owner's [fe todo], thus preventing release
      of the feature. [print_cr] is an expect-test-friendly version of [assert false]. It
      works with the normal expect-test workflow because it does not raise, and it
      prevents mistakenly releasing features that violate a required property. There is no
      need to 'X' a [CR require-failed]; simply fix the property that triggered the
      [print_cr] and re-run the test to restore the empty output. *)
  val print_cr
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> here:[%call_pos]
    -> Sexp.t
    -> unit

  (** [require bool] is a no-op if [bool = true], but if not, prints a [CR require-failed]
      similarly to [print_cr], with a message determined by the [if_false_then_print_s]
      argument, if any.

      [if_false_then_print_s] is useful for including information that may help debug the
      problem, but that would otherwise be too voluminous. [if_false_then_print_s] is lazy
      to avoid construction of the sexp except when needed. *)
  val require
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?if_false_then_print_s:Sexp.t Lazy.t
    -> here:[%call_pos]
    -> bool
    -> unit

  [%%template:
  [@@@kind.default k = (value, float64, bits32, bits64, word)]

  (** [require_equal] compares its two arguments using the equality predicate of the
      provided module. If the comparison fails, prints a message that renders the
      arguments as sexps. *)
  val require_equal
    : ('a : k).
    ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?if_false_then_print_s:Sexp.t Lazy.t
    -> ?message:string
    -> here:[%call_pos]
    -> (module With_equal with type t = ('a : k))
    -> 'a
    -> 'a
    -> unit

  (** Like [require_equal], but derives an equality predicate from a comparison function. *)
  val require_compare_equal
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?message:string
    -> here:[%call_pos]
    -> (module With_compare with type t = ('a : k))
    -> 'a
    -> 'a
    -> unit

  (** Like [require_equal] but instead requires that the arguments are *not* equal. *)
  val require_not_equal
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?if_false_then_print_s:Sexp.t Lazy.t
    -> ?message:string
    -> here:[%call_pos]
    -> (module With_equal with type t = ('a : k))
    -> 'a
    -> 'a
    -> unit

  (** Like [require_not_equal], but derives an equality predicate from a comparison
      function. *)
  val require_compare_not_equal
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?message:string
    -> here:[%call_pos]
    -> (module With_compare with type t = ('a : k))
    -> 'a
    -> 'a
    -> unit]

  (** Like [require_equal], but when equality fails produces a message including sexps of
      both [Set.diff first second] and [Set.diff second first] to aid in debugging. *)
  val require_sets_are_equal
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?names:string * string (** default is ["first", "second"] *)
    -> here:[%call_pos]
    -> ('elt, 'cmp) Set.t
    -> ('elt, 'cmp) Set.t
    -> unit

  (** [show_raise] calls [f ()] and prints the exception that it raises, or, if it doesn't
      raise, prints [did not raise]. [show_raise] ignores the result of [f] so that one
      doesn't have to put an [ignore] inside the body of an [f] that is expected to raise.
      [~hide_positions:true] operates as in [print_s], to make output less fragile. Using
      [~show_backtrace:true] will result in a CR in the expectation, but it's still
      available here as it is still valuable when initially writing tests and debugging. *)
  val show_raise
    :  ?hide_positions:bool (** default is [false] *)
    -> ?show_backtrace:bool (** default is [false] *)
    -> (unit -> _)
    -> unit

  (** [require_does_not_raise] is like [show_raise], but does not print anything if the
      function does not raise, and prints a CR along with the exception if it does raise.
      Unlike for [show_raise], the supplied function is required to return [unit] to avoid
      mistakes like incomplete partial application that silently would not raise, but for
      the wrong reason. *)
  val require_does_not_raise
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?show_backtrace:bool (** default is [false] *)
    -> here:[%call_pos]
    -> local_ (unit -> unit)
    -> unit

  (** [require_does_raise] is like [show_raise], but additionally prints a CR if the
      function does not raise. *)
  val require_does_raise
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] *)
    -> ?show_backtrace:bool (** default is [false] *)
    -> here:[%call_pos]
    -> local_ (unit -> _)
    -> unit

  (** [require_some option] is like [require (is_some option)], with improved output. If
      [option = None], it prints a CR. If [option = Some some] and [~print_some] is
      provided, it prints [print_some some]. *)
  val require_some
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?print_some:('some -> Sexp.t)
    -> here:[%call_pos]
    -> 'some option
    -> unit

  (** [require_none sexp_of_some option] is like [require (is_none option)], with improved
      output. If [option = Some some], it prints a CR including [sexp_of_some some]. If
      [option = None], it does not print. *)
  val require_none
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> here:[%call_pos]
    -> ('some -> Sexp.t)
    -> 'some option
    -> unit

  (** [require_ok or_error] is like [require (is_ok or_error)], with improved output. If
      [or_error = Error error], it prints a CR including [error]. If [or_error = Ok ok]
      and [~print_ok] is provided, it prints [print_ok ok]. *)
  val require_ok
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?print_ok:('ok -> Sexp.t)
    -> here:[%call_pos]
    -> 'ok Or_error.t
    -> unit

  (** [require_error sexp_of_ok or_error] is like [require (is_error or_error)], with
      improved output. If [or_error = Ok ok], it prints a CR including [sexp_of_ok ok]. If
      [or_error = Error error] and [print_error = true], it prints [error]. *)
  val require_error
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?print_error:bool (** default is [false] *)
    -> here:[%call_pos]
    -> ('ok -> Sexp.t)
    -> 'ok Or_error.t
    -> unit

  (** [require_ok_result sexp_of_error result] is like [require (is_ok or_error)], with
      improved output. If [result = Error error], it prints a CR including
      [sexp_of_error error]. If [result = Ok ok] and [~print_ok] is provided, it prints
      [print_ok ok]. *)
  val require_ok_result
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?print_ok:('ok -> Sexp.t)
    -> here:[%call_pos]
    -> ('error -> Sexp.t)
    -> ('ok, 'error) Result.t
    -> unit

  (** [require_error_result sexp_of_ok result] is like [require (is_error result)], with
      improved output. If [result = Ok ok], it prints a CR including [sexp_of_ok ok]. If
      [result = Error error] and [~print_error] is supplied, it prints
      [print_error error]. *)
  val require_error_result
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?print_error:('error -> Sexp.t)
    -> here:[%call_pos]
    -> ('ok -> Sexp.t)
    -> ('ok, 'error) Result.t
    -> unit

  (** [require_first print_second either] is like [require (is_first either)], with
      improved output. If [either = Second second], it prints a CR including
      [sexp_of_second second]. If [either = First first] and [~print_first] is provided,
      it prints [print_first first]. *)
  val require_first
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true otherwise] *)
    -> ?print_first:('first -> Sexp.t)
    -> here:[%call_pos]
    -> ('second -> Sexp.t)
    -> ('first, 'second) Either.t
    -> unit

  (** [require_second sexp_of_first either] is like [require (is_second either)], with
      improved output. If [either = First first], it prints a CR including
      [sexp_of_first first]. If [either = Second second] and [~print_second] is provided,
      it prints [print_second second]. *)
  val require_second
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true otherwise] *)
    -> ?print_second:('second -> Sexp.t)
    -> here:[%call_pos]
    -> ('first -> Sexp.t)
    -> ('first, 'second) Either.t
    -> unit

  (** Print string representations of the given values, and test that [to_string] /
      [of_string] round-trip. *)
  val print_and_check_stringable
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true otherwise] *)
    -> here:[%call_pos]
    -> (module With_stringable with type t = 'a)
    -> 'a list
    -> unit

  (** Print sexp representations of the given values, and test that [sexp_of_t] /
      [t_of_sexp] round-trip. *)
  val print_and_check_sexpable
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true otherwise] *)
    -> here:[%call_pos]
    -> (module With_sexpable with type t = 'a)
    -> 'a list
    -> unit

  (** Print the [With_round_trip] representations of the given values. If there are
      multiple representations, includes the [repr_name] of each in the output. Tests that
      [to_repr] and [of_repr] round-trip for each representation. *)
  val print_and_check_round_trip
    :  ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true otherwise] *)
    -> here:[%call_pos]
    -> (module With_equal with type t = 'a)
    -> (module With_round_trip with type t = 'a) list
    -> 'a list
    -> unit

  (** [quickcheck] is similar to [Base_quickcheck.Test.run], but

      1. [quickcheck] takes separate arguments for the values which
         [Base_quickcheck.Test.run] takes in a first-class module.

      2. [quickcheck] stops after the first iteration that raises or prints a CR, as
         detected by [on_print_cr]. *)
  val quickcheck
    :  here:[%call_pos]
    -> ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?seed:Quickcheck.Test.Config.Seed.t
    -> ?sizes:int Sequence.t
    -> ?trials:int
    -> ?shrinker:'a Quickcheck.Shrinker.t
    -> ?shrink_attempts:int
    -> ?examples:'a list
    -> sexp_of:('a -> Sexp.t)
    -> f:('a -> unit)
    -> 'a Quickcheck.Generator.t
    -> unit

  (** [quickcheck_m] is similar to [Base_quickcheck.Test.run]. It stops after the first
      iteration that raises or prints a CR, as detected by [on_print_cr]. *)
  val quickcheck_m
    :  here:[%call_pos]
    -> ?config:Base_quickcheck.Test.Config.t
         (** default is [Base_quickcheck.Test.default_config] *)
    -> ?cr:CR.t (** default is [CR] *)
    -> ?examples:'a list
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> (module Base_quickcheck.Test.S with type t = 'a)
    -> f:('a -> unit)
    -> unit

  (** [test_compare] uses quickcheck to test that a compare function is reflexive,
      asymmetric, and transitive. *)
  val test_compare
    :  here:[%call_pos]
    -> ?config:Base_quickcheck.Test.Config.t
         (** default is [Base_quickcheck.Test.default_config] *)
    -> ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> (module With_quickcheck_and_compare)
    -> unit

  (** [test_equal] uses quickcheck to test that an equal function is reflexive, symmetric,
      and transitive. *)
  val test_equal
    :  here:[%call_pos]
    -> ?config:Base_quickcheck.Test.Config.t
         (** default is [Base_quickcheck.Test.default_config] *)
    -> ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> (module With_quickcheck_and_equal)
    -> unit

  (** [test_compare_and_equal] uses quickcheck to test that compare and equal functions
      satisfy [test_compare] and [test_equal], and that their results are consistent with
      each other. *)
  val test_compare_and_equal
    :  here:[%call_pos]
    -> ?config:Base_quickcheck.Test.Config.t
         (** default is [Base_quickcheck.Test.default_config] *)
    -> ?cr:CR.t (** default is [CR] *)
    -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> (module With_quickcheck_and_compare_and_equal)
    -> unit

  (** [sexp_style] determines the sexp format used by [sexp_to_string], [print_s], and
      other functions in this module. Defaults to [Sexp_style.default_pretty]. *)
  val sexp_style : Sexp_style.t ref

  (** [on_print_cr] determines the behavior of all functions above that print CRs, such as
      [print_cr] and [require]. The rendered string form of the CR is passed to
      [!on_print_cr]. The default value is [print_endline]; this can be overridden to
      replace or extend the default behavior. For example, some testing harnesses may
      choose to abort a series of tests after the first CR is printed. *)
  val on_print_cr : (string -> unit) ref

  (** [with_sexp_round_floats] rounds floats when making sexp strings. The effect lasts
      for the duration of the function you pass, after which the previous behavior (full
      precision, by default) is restored. *)
  val with_sexp_round_floats : (unit -> 'a) -> significant_digits:int -> 'a

  (** Updates the currently collected expect test output to hide positions. Equivalent to
      [print_string ~hide_positions:true [%expect.output]]. *)
  val hide_positions_in_expect_test_output : here:[%call_pos] -> unit -> unit
end
