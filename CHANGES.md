## Release v0.16.0

Added support for locally allocated arguments to some functions via `[@local]` attribute.
Local allocation is an experimental compiler extension found at:
<https://github.com/ocaml-flambda/ocaml-jst>

In `Expect_test_helpers_base`:

* Added functions:
  * `am_running_expect_test`
  * `assert_am_running_expect_test`
  * `require_not_equal`
  * `require_compare_not_equal`
  * `print_and_check_round_trip`
  * `print_and_check_stringable`
  * `print_and_check_sexpable`

* Removed first-class module argument from `require_sets_are_equal`.

In `Expect_test_helpers_core`:

* Added `?print_limit` argument to `require_allocation_does_not_exceed` and
  `require_no_allocation`. This avoids very large output when there are thousands or more
  allocations.

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## v0.11

- Removed `print_bin_ios*` functions; use `print_and_check_stable_type`.

## v0.10

- Removed `show_allocation`; instead use `require_no_allocation` or
  `require_allocation_does_not_exceed`.

- Fixed the CR produced by the require function when a test fails to use the
  correct name for require.
