open! Base
open Expect_test_helpers_base

(* We test [raise_if_output_did_not_match] in its own file because we want to adjust
   [Expect_test_config] so that the inconsistent outputs test doesn't generate a C R. *)

module Expect_test_config = struct
  include Expect_test_config

  let upon_unreleasable_issue = `Warning_for_collector_testing
end

let%expect_test "raise_if_output_did_not_match" =
  print_endline "hello";
  [%expect {| hello |}];
  require_does_not_raise (fun () ->
    raise_if_output_did_not_match ~message:"lorem ipsum" ());
  let expect_hello str =
    print_string str;
    [%expect
      {|
      (* expect_test: Test ran multiple times with different test outputs *)
      ============================ Output 1 / 2 ============================
      hello
      ============================ Output 2 / 2 ============================
      goodbye
      |}]
  in
  expect_hello "hello";
  expect_hello "goodbye";
  require_does_raise ~hide_positions:true (fun () -> raise_if_output_did_not_match ());
  [%expect
    {|
    ("[raise_if_output_did_not_match]: encountered a mismatch"
     lib/expect_test_helpers/base/test/test_raise_if_output_did_not_match.ml:LINE:COL)
    |}];
  require_does_raise ~hide_positions:true (fun () ->
    raise_if_output_did_not_match ~message:"dolor sit amet" ());
  [%expect
    {|
    ("dolor sit amet"
     "[raise_if_output_did_not_match]: encountered a mismatch"
     lib/expect_test_helpers/base/test/test_raise_if_output_did_not_match.ml:LINE:COL)
    |}]
;;
