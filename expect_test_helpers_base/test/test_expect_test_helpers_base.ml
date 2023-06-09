open! Base
open! Stdio
open! Expect_test_helpers_base

(* A dummy location for testing purposes *)
let nowhere = { Lexing.pos_fname = "test"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let%expect_test "[am_running_expect_test] and [assert_am_running_expect_test] when true" =
  require [%here] (am_running_expect_test ());
  assert_am_running_expect_test [%here];
  [%expect {| |}]
;;

let%test_unit "[am_running_expect_test] and [assert_am_running_expect_test] when false" =
  assert (not (am_running_expect_test ()));
  match assert_am_running_expect_test [%here] with
  | () -> assert false
  | exception exn ->
    [%test_result: string]
      (hide_positions_in_string (Exn.to_string exn))
      ~expect:
        "(\"This code should be run inside an expect test; currently, it is running \
         outside an expect test.\"\n\
        \  lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL)"
;;

let%expect_test "multiple calls to [print_s] create multiple lines" =
  print_s [%message "hello"];
  print_s [%message "there"];
  [%expect {|
    hello
    there
  |}]
;;

let%expect_test "[print_s ~hide_positions:true]" =
  print_s ~hide_positions:true [%message [%here] [%here]];
  [%expect
    {|
      (lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL
       lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[print_string ~hide_positions:true]" =
  print_string ~hide_positions:true (Sexp.to_string_hum [%message [%here] [%here]]);
  [%expect
    {|
      (lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL
       lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[print_endline ~hide_positions:true]" =
  print_endline ~hide_positions:true (Sexp.to_string_hum [%message [%here] [%here]]);
  [%expect
    {|
      (lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL
       lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[~hide_positions:true] for line number from [%of_sexp]" =
  show_raise ~hide_positions:true (fun () -> [%of_sexp: int * int] (List []));
  [%expect
    {|
    (raised (
      Of_sexp_error
      "test_expect_test_helpers_base.ml line LINE: (int * int)_of_sexp: tuple of size 2 expected"
      (invalid_sexp ()))) |}]
;;

let%expect_test "[sexp_style]" =
  let sexp =
    List.init 6 ~f:(fun x -> List.init x ~f:(fun y -> List.init y ~f:(fun z -> x, y, z)))
    |> [%sexp_of: (int * int * int) list list list]
  in
  let test style =
    Ref.set_temporarily sexp_style style ~f:(fun () ->
      print_s sexp;
      require
        [%here]
        (String.is_suffix (sexp_to_string sexp) ~suffix:"\n")
        ~if_false_then_print_s:(lazy [%message "no endline"]))
  in
  test To_string_mach;
  [%expect
    {|
      (()(())(()((2 1 0)))(()((3 1 0))((3 2 0)(3 2 1)))(()((4 1 0))((4 2 0)(4 2 1))((4 3 0)(4 3 1)(4 3 2)))(()((5 1 0))((5 2 0)(5 2 1))((5 3 0)(5 3 1)(5 3 2))((5 4 0)(5 4 1)(5 4 2)(5 4 3)))) |}];
  test To_string_hum;
  [%expect
    {|
    (() (()) (() ((2 1 0))) (() ((3 1 0)) ((3 2 0) (3 2 1)))
     (() ((4 1 0)) ((4 2 0) (4 2 1)) ((4 3 0) (4 3 1) (4 3 2)))
     (() ((5 1 0)) ((5 2 0) (5 2 1)) ((5 3 0) (5 3 1) (5 3 2))
      ((5 4 0) (5 4 1) (5 4 2) (5 4 3)))) |}];
  test Sexp_style.simple_pretty;
  [%expect
    {|
    (()
     (())
     (() ((2 1 0)))
     (() ((3 1 0)) ((3 2 0) (3 2 1)))
     (() ((4 1 0)) ((4 2 0) (4 2 1)) ((4 3 0) (4 3 1) (4 3 2)))
     (()
      ((5 1 0))
      ((5 2 0) (5 2 1))
      ((5 3 0) (5 3 1) (5 3 2))
      ((5 4 0) (5 4 1) (5 4 2) (5 4 3)))) |}];
  test Sexp_style.default_pretty;
  [%expect
    {|
    (()
     (())
     (() ((2 1 0)))
     (()
      ((3 1 0))
      ((3 2 0)
       (3 2 1)))
     (()
      ((4 1 0))
      ((4 2 0)
       (4 2 1))
      ((4 3 0)
       (4 3 1)
       (4 3 2)))
     (()
      ((5 1 0))
      ((5 2 0)
       (5 2 1))
      ((5 3 0)
       (5 3 1)
       (5 3 2))
      ((5 4 0)
       (5 4 1)
       (5 4 2)
       (5 4 3)))) |}]
;;

let%expect_test "[show_raise], no exception" =
  show_raise ~hide_positions:true (fun () -> ());
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "[show_raise], raises hiding positions" =
  show_raise ~hide_positions:true (fun () -> raise_s [%message [%here]]);
  [%expect
    {|
    (raised
     lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[show_raise] with a deep stack" =
  let rec loop n = if n = 0 then failwith "raising" else 1 + loop (n - 1) in
  show_raise (fun () -> loop 13);
  [%expect {|
    (raised (Failure raising)) |}]
;;

let%expect_test "[show_raise] ignores return value" =
  show_raise (fun () -> 13);
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "[require] true prints nothing" =
  require [%here] true;
  [%expect {||}]
;;

let%expect_test "[cr]" =
  print_cr [%here] [%message "some message"] ~cr:Comment;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "some message" |}]
;;

let%expect_test "[require] false respects [~cr] and default [~hide_positions]" =
  require [%here] false ~cr:Comment ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL |}]
;;

let%expect_test "[require false] on non-comment [~cr] values includes instructions" =
  require [%here] false ~cr:CR_someday ~if_false_then_print_s:(lazy [%message [%here]]);
  [%expect
    {|
    lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL |}]
;;

let%expect_test "[require_equal] success" =
  require_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect {||}]
;;

let%expect_test "[require_equal] failure" =
  require_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are not equal" 1 2) |}]
;;

let%expect_test "[require_equal] failure with [~message]" =
  require_equal [%here] (module Int) ~cr:Comment 1 2 ~message:"The sky is falling!";
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("The sky is falling!" 1 2) |}]
;;

let%expect_test "[require_equal] failure with [~if_false_then_print_s]" =
  require_equal
    [%here]
    (module Int)
    ~cr:Comment
    1
    2
    ~if_false_then_print_s:(lazy [%message "The sky is falling!"]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are not equal" 1 2 "The sky is falling!") |}]
;;

let%expect_test "[require_compare_equal] success" =
  require_compare_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect {||}]
;;

let%expect_test "[require_compare_equal] failure" =
  require_compare_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are not equal" 1 2) |}]
;;

let%expect_test "[require_compare_equal] failure with [~message]" =
  require_compare_equal
    [%here]
    (module Int)
    ~cr:Comment
    1
    2
    ~message:"The sky is falling!";
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("The sky is falling!" 1 2) |}]
;;

let%expect_test "[require_not_equal] success" =
  require_not_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect {||}]
;;

let%expect_test "[require_not_equal] failure" =
  require_not_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are equal" 1 1) |}]
;;

let%expect_test "[require_not_equal] failure with [~message]" =
  require_not_equal [%here] (module Int) ~cr:Comment 1 1 ~message:"The sky is falling!";
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("The sky is falling!" 1 1) |}]
;;

let%expect_test "[require_not_equal] failure with [~if_false_then_print_s]" =
  require_not_equal
    [%here]
    (module Int)
    ~cr:Comment
    1
    1
    ~if_false_then_print_s:(lazy [%message "The sky is falling!"]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are equal" 1 1 "The sky is falling!") |}]
;;

let%expect_test "[require_compare_not_equal] success" =
  require_compare_not_equal [%here] (module Int) ~cr:Comment 1 2;
  [%expect {||}]
;;

let%expect_test "[require_compare_not_equal] failure" =
  require_compare_not_equal [%here] (module Int) ~cr:Comment 1 1;
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are equal" 1 1) |}]
;;

let%expect_test "[require_compare_not_equal] failure with [~message]" =
  require_compare_not_equal
    [%here]
    (module Int)
    ~cr:Comment
    1
    1
    ~message:"The sky is falling!";
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("The sky is falling!" 1 1) |}]
;;

let%expect_test "[require_does_not_raise], no exception" =
  require_does_not_raise [%here] ~hide_positions:true (fun () -> ());
  [%expect {| |}]
;;

let%expect_test "[require_does_not_raise], raises hiding positions" =
  require_does_not_raise [%here] ~cr:Comment (fun () -> raise_s [%message [%here]]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised"
     lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL) |}]
;;

let%expect_test "[require_does_not_raise] with a deep stack" =
  let rec loop n = if n = 0 then failwith "raising" else 1 + loop (n - 1) in
  require_does_not_raise [%here] ~cr:Comment (fun () -> ignore (loop 13 : int));
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised" (Failure raising)) |}]
;;

let%expect_test "[require_does_raise] failure" =
  require_does_raise nowhere ~cr:Comment (fun () -> ());
  [%expect {|
    (* require-failed: test:0:0. *)
    "did not raise" |}]
;;

let%expect_test "[require_does_raise] success" =
  require_does_raise [%here] (fun () -> raise_s [%message "Boom!"]);
  [%expect {| Boom! |}]
;;

let%expect_test "[require_does_raise] success" =
  require_does_raise [%here] ~cr:Comment (fun () ->
    raise_s [%message "here: " (Source_code_position.to_string nowhere)]);
  [%expect {| ("here: " test:0:0) |}]
;;

let%expect_test "[require_does_raise ~hide_positions:true] success" =
  require_does_raise [%here] ~hide_positions:true (fun () -> raise_s [%message [%here]]);
  [%expect
    {| lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL |}]
;;

include struct
  open struct
    let sexp_const s () = [%message s]
    let print_error = sexp_const "error"
    let print_first = sexp_const "first"
    let print_ok = sexp_const "ok"
    let print_second = sexp_const "second"
    let print_some = sexp_const "some"
  end

  let%expect_test "[require_some]" =
    require_some [%here] (Some ());
    [%expect {| |}];
    require_some [%here] (Some ()) ~print_some;
    [%expect {| some |}];
    require_some [%here] ~cr:Comment None;
    (* It's silly to print the [None], but it doesn't seem worth special-casing. *)
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [None]" ()) |}]
  ;;

  let%expect_test "[require_none]" =
    require_none [%here] [%sexp_of: _] None;
    [%expect {| |}];
    require_none [%here] print_some ~cr:Comment (Some ());
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Some]" some) |}]
  ;;

  let%expect_test "[require_first]" =
    require_first [%here] [%sexp_of: _] (First ());
    [%expect {| |}];
    require_first [%here] [%sexp_of: _] ~print_first (First ());
    [%expect {| first |}];
    require_first [%here] print_second ~cr:Comment (Second ());
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Second]" second) |}]
  ;;

  let%expect_test "[require_second]" =
    require_second [%here] print_first (Second ());
    [%expect {| |}];
    require_second [%here] print_first (Second ()) ~print_second;
    [%expect {| second |}];
    require_second [%here] ~cr:Comment print_first (First ());
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [First]" first) |}];
    require_second [%here] ~cr:Comment print_first (First ()) ~print_second;
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [First]" first) |}]
  ;;

  let%expect_test "[require_ok_result]" =
    require_ok_result [%here] print_error (Ok ());
    [%expect {| |}];
    require_ok_result [%here] print_error (Ok ()) ~print_ok;
    [%expect {| ok |}];
    require_ok_result [%here] ~cr:Comment print_error (Error ());
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Error]" error) |}];
    require_ok_result [%here] ~cr:Comment print_error (Error ()) ~print_ok;
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Error]" error) |}]
  ;;

  let%expect_test "[require_error_result]" =
    require_error_result [%here] print_ok (Error ());
    [%expect {| |}];
    require_error_result [%here] print_ok (Error ()) ~print_error;
    [%expect {| error |}];
    require_error_result [%here] ~cr:Comment print_ok (Ok ());
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Ok]" ok) |}];
    require_error_result [%here] ~cr:Comment print_ok (Ok ()) ~print_error;
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Ok]" ok) |}]
  ;;

  let%expect_test "[require_ok]" =
    require_ok [%here] (Ok ());
    [%expect {| |}];
    require_ok [%here] (Ok ()) ~print_ok;
    [%expect {| ok |}];
    require_ok [%here] ~cr:Comment (Or_error.error_string "arstneio");
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Error]" arstneio) |}]
  ;;

  let%expect_test "[require_error]" =
    require_error [%here] print_ok (Or_error.error_string "arstneio");
    [%expect {| |}];
    require_error [%here] print_ok (Or_error.error_string "arstneio") ~print_error:true;
    [%expect {| arstneio |}];
    require_error [%here] ~cr:Comment print_ok (Ok ());
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpected [Ok]" ok) |}]
  ;;
end

let%expect_test "[replace]" =
  "/tmp/dir.tmp.123456/file.txt copied from /jenga/root/app/foo/file.txt"
  |> replace ~pattern:"/tmp/dir.tmp.123456" ~with_:"$TMP"
  |> replace ~pattern:"/jenga/root" ~with_:"$ROOT"
  |> print_endline;
  [%expect {| $TMP/file.txt copied from $ROOT/app/foo/file.txt |}]
;;

let%expect_test "[replace_s]" =
  [%sexp
    "copied file"
  , { dst = "/tmp/dir.tmp.123456/file.txt"; src = "/jenga/root/app/foo/file.txt" }]
  |> replace_s ~pattern:"/tmp/dir.tmp.123456" ~with_:"$TMP"
  |> replace_s ~pattern:"/jenga/root" ~with_:"$ROOT"
  |> print_s;
  [%expect
    {|
    ("copied file" (
      (dst $TMP/file.txt)
      (src $ROOT/app/foo/file.txt))) |}]
;;

let%expect_test "hide_temp_files_in_string" =
  "/usr/local/home/non-user.tmp.abcXYZ/file.tmp.r2c3p0.gz"
  |> hide_temp_files_in_string
  |> print_endline;
  [%expect {| /usr/local/home/non-user.tmp.RANDOM/file.tmp.RANDOM.gz |}]
;;

let%expect_test "[require_sets_are_equal] success" =
  require_sets_are_equal [%here] (Set.empty (module Int)) (Set.empty (module Int));
  [%expect {| |}];
  require_sets_are_equal
    [%here]
    (Set.of_list (module Int) [ 1; 2; 3 ])
    (Set.of_list (module Int) [ 3; 2; 1 ]);
  [%expect {| |}]
;;

let%expect_test "[require_sets_are_equal] failure" =
  require_sets_are_equal
    [%here]
    ~cr:Comment
    (Set.of_list (module Int) [ 1; 2 ])
    (Set.of_list (module Int) [ 2; 3 ]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal"
      ("in first but not in second" (1))
      ("in second but not in first" (3))) |}]
;;

let%expect_test "[require_sets_are_equal] failure with extras only in first" =
  require_sets_are_equal
    [%here]
    ~cr:Comment
    (Set.of_list (module Int) [ 1; 2 ])
    (Set.of_list (module Int) [ 2 ]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal" ("in first but not in second" (1))) |}]
;;

let%expect_test "[require_sets_are_equal] failure with extras only in second" =
  require_sets_are_equal
    [%here]
    ~cr:Comment
    (Set.of_list (module Int) [ 2 ])
    (Set.of_list (module Int) [ 2; 3 ]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal" ("in second but not in first" (3))) |}]
;;

let%expect_test "[require_sets_are_equal] failure with names" =
  require_sets_are_equal
    [%here]
    (Set.of_list (module Int) [ 1; 2 ])
    (Set.of_list (module Int) [ 2; 3 ])
    ~cr:Comment
    ~names:("expected", "actual");
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("sets are not equal"
      ("in expected but not in actual" (1))
      ("in actual but not in expected" (3))) |}]
;;

let%expect_test "[on_print_cr]" =
  let cr = CR.Comment in
  let hide_positions = true in
  let run () =
    print_cr [%here] ~cr ~hide_positions [%message "unconditional message"];
    require
      [%here]
      ~cr
      ~hide_positions
      false
      ~if_false_then_print_s:(lazy [%message "conditional message"]);
    require
      [%here]
      ~cr
      ~hide_positions
      true
      ~if_false_then_print_s:(lazy [%message "elided message"])
  in
  let default = !on_print_cr in
  run ();
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "unconditional message"
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "conditional message" |}];
  on_print_cr := ignore;
  run ();
  [%expect {||}];
  (on_print_cr := fun string -> print_endline (String.uppercase string));
  run ();
  [%expect
    {|
    (* REQUIRE-FAILED: LIB/EXPECT_TEST_HELPERS/BASE/TEST/TEST_EXPECT_TEST_HELPERS_BASE.ML:LINE:COL. *)
    "UNCONDITIONAL MESSAGE"
    (* REQUIRE-FAILED: LIB/EXPECT_TEST_HELPERS/BASE/TEST/TEST_EXPECT_TEST_HELPERS_BASE.ML:LINE:COL. *)
    "CONDITIONAL MESSAGE" |}];
  on_print_cr := default;
  run ();
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "unconditional message"
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    "conditional message" |}]
;;

let%expect_test "[quickcheck] success" =
  quickcheck
    [%here]
    Base_quickcheck.quickcheck_generator_int
    ~sexp_of:Int.sexp_of_t
    ~f:ignore;
  [%expect {||}]
;;

(* Quickcheck pseudo-random generation is different on 32-bit and 64-bit, so we only run
   Quickcheck failure tests on 64-bit builds. *)

let%expect_test ("[quickcheck] failure" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck
    [%here]
    ~cr
    Base_quickcheck.quickcheck_generator_int
    ~sexp_of:Int.sexp_of_t
    ~f:(fun int ->
      require [%here] ~cr (int > 100) ~if_false_then_print_s:(lazy [%message "BAD"]));
  [%expect
    {|
    ("quickcheck: test failed" (input -15508265059))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    BAD |}]
;;

let%expect_test ("[quickcheck] failure with multiple CRs" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck
    [%here]
    ~cr
    Base_quickcheck.quickcheck_generator_int
    ~sexp_of:Int.sexp_of_t
    ~f:(fun _ ->
      print_cr [%here] ~cr [%message "first"];
      require [%here] ~cr false ~if_false_then_print_s:(lazy [%message "second"]));
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    first
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    second |}]
;;

let%expect_test ("[quickcheck] raised exception" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  require_does_not_raise [%here] (fun () ->
    quickcheck
      [%here]
      ~cr
      Base_quickcheck.quickcheck_generator_int
      ~sexp_of:Int.sexp_of_t
      ~f:(fun int -> if int > 100 then raise_s [%message "BAD"]));
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised" BAD) |}]
;;

let%expect_test ("[quickcheck] failure with shrinker" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck
    [%here]
    ~cr
    (Base_quickcheck.Generator.return 10)
    ~sexp_of:[%sexp_of: int]
    ~shrinker:(Base_quickcheck.Shrinker.create (fun int -> Sequence.singleton (int - 1)))
    ~f:(fun int ->
      require
        [%here]
        ~cr
        (int <= 0)
        ~if_false_then_print_s:(lazy [%message "positive" ~_:(int : int)]));
  [%expect
    {|
    ("quickcheck: test failed" (input 1))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    (positive 1) |}]
;;

module Int_for_quickcheck = struct
  open Base_quickcheck.Export

  type t = int [@@deriving quickcheck, sexp_of]
end

let%expect_test "[quickcheck_m] success" =
  quickcheck_m [%here] (module Int_for_quickcheck) ~f:ignore;
  [%expect {||}]
;;

(* Quickcheck pseudo-random generation is different on 32-bit and 64-bit, so we only run
   Quickcheck failure tests on 64-bit builds. *)

let%expect_test ("[quickcheck_m] failure" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck_m
    [%here]
    ~cr
    (module Int_for_quickcheck)
    ~f:(fun int ->
      require [%here] ~cr (int > 100) ~if_false_then_print_s:(lazy [%message "BAD"]));
  [%expect
    {|
    ("quickcheck: test failed" (input -15508265059))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    BAD |}]
;;

let%expect_test ("[quickcheck_m] failure with multiple CRs" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck_m
    [%here]
    ~cr
    (module Int_for_quickcheck)
    ~f:(fun _ ->
      print_cr [%here] ~cr [%message "first"];
      require [%here] ~cr false ~if_false_then_print_s:(lazy [%message "second"]));
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    first
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    second |}]
;;

let%expect_test ("[quickcheck_m] raised exception" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  require_does_not_raise [%here] (fun () ->
    quickcheck_m
      [%here]
      ~cr
      (module Int_for_quickcheck)
      ~f:(fun int -> if int > 100 then raise_s [%message "BAD"]));
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised" BAD) |}]
;;

let%expect_test ("[quickcheck_m] failure with shrinker" [@tags "64-bits-only"]) =
  let cr = CR.Comment in
  quickcheck_m
    [%here]
    ~cr
    (module struct
      type t = int [@@deriving sexp_of]

      let quickcheck_generator = Base_quickcheck.Generator.return 10

      let quickcheck_shrinker =
        Base_quickcheck.Shrinker.create (fun int -> Sequence.singleton (int - 1))
      ;;
    end)
    ~f:(fun int ->
      require
        [%here]
        ~cr
        (int <= 0)
        ~if_false_then_print_s:(lazy [%message "positive" ~_:(int : int)]));
  [%expect
    {|
    ("quickcheck: test failed" (input 1))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    (positive 1) |}]
;;

let%expect_test "Phys_equal" =
  require_equal [%here] (module Phys_equal (Int)) 1 1;
  require_equal
    [%here]
    ~cr:Comment
    (module Phys_equal (struct
         type t = string option [@@deriving sexp_of]
       end))
    (Some "foo")
    (Some ("fo" ^ "o"));
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("values are not equal"
      (foo)
      (foo)) |}]
;;

let%test_module _ =
  (module struct
    let%expect_test "[%expect.output]" =
      let output =
        print_endline "This is a sentence.";
        [%expect.output]
      in
      [%expect {| |}];
      print_string output;
      [%expect {| This is a sentence. |}]
    ;;

    let%expect_test "expect_test_output" =
      let output =
        print_endline "This is a sentence.";
        expect_test_output [%here]
      in
      [%expect {| |}];
      print_string output;
      [%expect {| This is a sentence. |}]
    ;;

    let%expect_test "expect_test_output with source location from different file" =
      let output =
        print_endline "This is a sentence.";
        expect_test_output
          { pos_fname = "__nonexistent__"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
      in
      [%expect {| |}];
      print_string output;
      [%expect {| This is a sentence. |}]
    ;;
  end)
;;

let%expect_test "smash_sexp" =
  {|
((name (Ok "John Jacob Jingleheimer Schmidt"))
 (id (Error "That's my name, too!"))
 (contents
   (Ok
    ((date (Error "I have no idea."))
     (time (Ok "Whenever he goes out."))))))
|}
  |> Parsexp.Single.parse_string_exn
  |> smash_sexp ~f:(function
    | List [ Atom "Ok"; ok ] -> ok
    | sexp -> sexp)
  |> print_s;
  [%expect
    {|
    ((name "John Jacob Jingleheimer Schmidt")
     (id (Error "That's my name, too!"))
     (contents ((date (Error "I have no idea.")) (time "Whenever he goes out.")))) |}]
;;

let%expect_test "remove_backtrace" =
  {|
((backtrace
 ("Raised at Base__Error.raise in file \"error.ml\" (inlined), line 9, characters 14-30"
  "Called from Base__Error.raise_s in file \"error.ml\", line 10, characters 19-40"
  "Called from Base__Result.try_with in file \"result.ml\", line 227, characters 9-15")))
|}
  |> Parsexp.Single.parse_string_exn
  |> remove_backtraces
  |> print_s;
  [%expect {| ((backtrace ("ELIDED BACKTRACE"))) |}]
;;

let%expect_test "print_and_check_stringable" =
  print_and_check_stringable
    ~cr:Comment
    [%here]
    (module Int32)
    Int32.[ min_value; minus_one; zero; one; max_value ];
  [%expect {|
    -2147483648
    -1
    0
    1
    2147483647 |}]
;;

let%expect_test "print_and_check_sexpable" =
  print_and_check_sexpable
    ~cr:Comment
    [%here]
    (module Int32)
    Int32.[ min_value; minus_one; zero; one; max_value ];
  [%expect {|
    -2147483648
    -1
    0
    1
    2147483647 |}]
;;

let%expect_test "print_and_check_round_trip" =
  (* conversion works for all int64s *)
  let module Int64_as_string = struct
    type t = int64
    type repr = string [@@deriving sexp_of]

    let to_repr = Int64.to_string
    let of_repr = Int64.of_string
    let repr_name = "string"
  end
  in
  (* conversion fails to round-trip for some int64s *)
  let module Int64_as_int32_trunc = struct
    type t = int64
    type repr = int32 [@@deriving sexp_of]

    let to_repr = Int64.to_int32_trunc
    let of_repr = Int64.of_int32
    let repr_name = "int32"
  end
  in
  (* conversion raises for some int64s *)
  let module Int64_as_int63_exn = struct
    type t = int64
    type repr = Int63.t [@@deriving sexp_of]

    let to_repr x =
      try Int63.of_int64_exn x with
      | Failure string ->
        (* error message varies by platform, so we normalize it *)
        failwith (String.substr_replace_all string ~pattern:" int " ~with_:" int63 ")
    ;;

    let of_repr = Int63.to_int64
    let repr_name = "int63"
  end
  in
  (* test all three conversions *)
  let test list =
    print_and_check_round_trip
      ~cr:Comment
      [%here]
      (module Int64)
      [ (module Int64_as_string)
      ; (module Int64_as_int32_trunc)
      ; (module Int64_as_int63_exn)
      ]
      list
  in
  (* successful round-trip *)
  test [ -1L; 0L; 1L ];
  [%expect
    {|
    ((string -1)
     (int32  -1)
     (int63  -1))
    ((string 0)
     (int32  0)
     (int63  0))
    ((string 1)
     (int32  1)
     (int63  1)) |}];
  (* unsuccessful round-trip *)
  test
    [ Int64.pred (Int64.of_int32 Int32.min_value)
    ; Int64.succ (Int64.of_int32 Int32.max_value)
    ];
  [%expect
    {|
    ((string -2147483649)
     (int32  2147483647)
     (int63  -2147483649))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("int32 serialization failed to round-trip"
      (original        -2147483649)
      (int32           2147483647)
      (int32_roundtrip 2147483647))
    ((string 2147483648)
     (int32  -2147483648)
     (int63  2147483648))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("int32 serialization failed to round-trip"
      (original        2147483648)
      (int32           -2147483648)
      (int32_roundtrip -2147483648)) |}];
  (* conversion raises *)
  test [ Int64.min_value; Int64.max_value ];
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("unexpectedly raised" (
      Failure
      "conversion from int64 to int63 failed: -9223372036854775808 is out of range")) |}];
  ()
;;

let%expect_test ("test_compare" [@tags "64-bits-only"]) =
  let open Base_quickcheck.Export in
  (* success *)
  test_compare
    [%here]
    ~cr:Comment
    (module struct
      type t = int [@@deriving compare, quickcheck, sexp_of]
    end);
  [%expect {| |}];
  (* failure *)
  test_compare
    [%here]
    ~cr:Comment
    (module struct
      type t = int [@@deriving quickcheck, sexp_of]

      let compare x _ = x
    end);
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not reflexive"
      (x           76753)
      (compare_x_x 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not asymmetric"
      (x           76753)
      (y           76753)
      (compare_x_y 76753)
      (compare_y_x 76753))
    ("quickcheck: test failed" (input (-1710895137 -225617)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not asymmetric"
      (x           -1710895137)
      (y           -225617)
      (compare_x_y -1710895137)
      (compare_y_x -225617))
    ("quickcheck: test failed" (input (0 7408 29976914579383)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not transitive"
      (x           0)
      (y           7408)
      (z           29976914579383)
      (compare_x_y 0)
      (compare_y_z 7408)
      (compare_x_z 0))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not transitive"
      (x           0)
      (y           29976914579383)
      (z           7408)
      (compare_x_y 0)
      (compare_y_z 29976914579383)
      (compare_x_z 0)) |}]
;;

let%expect_test ("test_equal" [@tags "64-bits-only"]) =
  let open Base_quickcheck.Export in
  (* success *)
  test_equal
    [%here]
    ~cr:Comment
    (module struct
      type t = int [@@deriving equal, quickcheck, sexp_of]
    end);
  [%expect {| |}];
  (* failure *)
  test_equal
    [%here]
    ~cr:Comment
    (module struct
      type t = int [@@deriving quickcheck, sexp_of]

      let equal x _ = x < 0
    end);
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not reflexive"
      (x         76753)
      (equal_x_x false))
    ("quickcheck: test failed" (input (-15508265059 76753)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not symmetric"
      (x         -15508265059)
      (y         76753)
      (equal_x_y true)
      (equal_y_x false))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not transitive"
      (x         -15508265059)
      (y         76753)
      (z         -15508265059)
      (equal_x_y true)
      (equal_y_z false)
      (equal_x_z true))
    ("quickcheck: test failed" (input (-54193148208643064 -15508265059 76753)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not transitive"
      (x         -54193148208643064)
      (y         76753)
      (z         -15508265059)
      (equal_x_y true)
      (equal_y_z false)
      (equal_x_z true)) |}]
;;

let%expect_test ("test_compare_and_equal" [@tags "64-bits-only"]) =
  let open Base_quickcheck.Export in
  (* success *)
  test_compare_and_equal
    [%here]
    ~cr:Comment
    (module struct
      type t = int [@@deriving compare, equal, quickcheck, sexp_of]
    end);
  [%expect {| |}];
  (* failure *)
  test_compare_and_equal
    [%here]
    ~cr:Comment
    (module struct
      type t = int [@@deriving quickcheck, sexp_of]

      let compare x _ = x
      let equal x _ = x < 0
    end);
  [%expect
    {|
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not reflexive"
      (x           76753)
      (compare_x_x 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not asymmetric"
      (x           76753)
      (y           76753)
      (compare_x_y 76753)
      (compare_y_x 76753))
    ("quickcheck: test failed" (input (-1710895137 -225617)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not asymmetric"
      (x           -1710895137)
      (y           -225617)
      (compare_x_y -1710895137)
      (compare_y_x -225617))
    ("quickcheck: test failed" (input (0 7408 29976914579383)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not transitive"
      (x           0)
      (y           7408)
      (z           29976914579383)
      (compare_x_y 0)
      (compare_y_z 7408)
      (compare_x_z 0))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] is not transitive"
      (x           0)
      (y           29976914579383)
      (z           7408)
      (compare_x_y 0)
      (compare_y_z 29976914579383)
      (compare_x_z 0))
    ("quickcheck: test failed" (input 76753))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not reflexive"
      (x         76753)
      (equal_x_x false))
    ("quickcheck: test failed" (input (-15508265059 76753)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not symmetric"
      (x         -15508265059)
      (y         76753)
      (equal_x_y true)
      (equal_y_x false))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not transitive"
      (x         -15508265059)
      (y         76753)
      (z         -15508265059)
      (equal_x_y true)
      (equal_y_z false)
      (equal_x_z true))
    ("quickcheck: test failed" (input (-54193148208643064 -15508265059 76753)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[equal] is not transitive"
      (x         -54193148208643064)
      (y         76753)
      (z         -15508265059)
      (equal_x_y true)
      (equal_y_z false)
      (equal_x_z true))
    ("quickcheck: test failed" (input (-15508265059 76753)))
    (* require-failed: lib/expect_test_helpers/base/test/test_expect_test_helpers_base.ml:LINE:COL. *)
    ("[compare] and [equal] do not agree"
      (x           -15508265059)
      (y           76753)
      (compare_x_y -15508265059)
      (equal_x_y   true)) |}]
;;
