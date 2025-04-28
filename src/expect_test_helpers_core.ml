open! Core
include Expect_test_helpers_base
include Expect_test_helpers_core_intf

module Allocation_limit = struct
  include Allocation_limit

  let is_ok t ~major_words_allocated ~minor_words_allocated =
    match t with
    | Major_words n -> major_words_allocated <= n
    | Minor_words n -> major_words_allocated = 0 && minor_words_allocated <= n
  ;;

  let show_major_words = function
    | Major_words _ -> true
    | Minor_words _ -> false
  ;;
end

module type Int63able = sig
  type t

  val to_int63 : t -> Int63.t
  val of_int63_exn : Int63.t -> t
end

let print_and_check_stable_internal
  (type a)
  ?cr
  ?hide_positions
  ?max_binable_length
  ?(here = Stdlib.Lexing.dummy_pos)
  (module M : Stable_without_comparator with type t = a)
  (int63able : (module Int63able with type t = a) option)
  list
  =
  let module M = struct
    include M

    let equal = [%compare.equal: t]
  end
  in
  print_s
    ?hide_positions
    [%message
      "" ~bin_shape_digest:(Bin_prot.Shape.eval_to_digest_string M.bin_shape_t : string)];
  let sexp_m =
    (module struct
      type t = M.t
      type repr = Sexp.t [@@deriving sexp_of]

      let to_repr = M.sexp_of_t
      let of_repr = M.t_of_sexp
      let repr_name = "sexp"
    end : With_round_trip
      with type t = a)
  in
  let bin_io_m =
    (module struct
      type t = M.t
      type repr = string [@@deriving sexp_of]

      let to_repr =
        match max_binable_length with
        | None -> Binable.to_string (module M)
        | Some max_binable_length ->
          fun original ->
            let bin_io = Binable.to_string (module M) original in
            let bin_io_length = String.length bin_io in
            require
              ?cr
              ?hide_positions
              ~here
              (bin_io_length <= max_binable_length)
              ~if_false_then_print_s:
                (lazy
                  [%message
                    "bin-io serialization exceeds max binable length"
                      (original : M.t)
                      (bin_io : string)
                      (bin_io_length : int)
                      (max_binable_length : int)]);
            bin_io
      ;;

      let of_repr = Binable.of_string (module M)
      let repr_name = "bin-io"
    end : With_round_trip
      with type t = a)
  in
  let int63able_m =
    let%map.Option (module I) = int63able in
    (module struct
      type t = M.t
      type repr = Int63.t [@@deriving sexp_of]

      let to_repr = I.to_int63
      let of_repr = I.of_int63_exn
      let repr_name = "int63"
    end : With_round_trip
      with type t = a)
  in
  print_and_check_round_trip
    ?cr
    ?hide_positions
    ~here
    (module M)
    (List.concat [ [ sexp_m; bin_io_m ]; Option.to_list int63able_m ])
    list
;;

let print_and_check_stable_type
  (type a)
  ?cr
  ?hide_positions
  ?max_binable_length
  ?(here = Stdlib.Lexing.dummy_pos)
  (module M : Stable_without_comparator with type t = a)
  list
  =
  print_and_check_stable_internal
    ?cr
    ?hide_positions
    ?max_binable_length
    ~here
    (module M)
    None
    list
;;

let print_and_check_stable_int63able_type
  (type a)
  ?cr
  ?hide_positions
  ?max_binable_length
  ?(here = Stdlib.Lexing.dummy_pos)
  (module M : Stable_int63able_without_comparator with type t = a)
  list
  =
  print_and_check_stable_internal
    ?cr
    ?hide_positions
    ?max_binable_length
    ~here
    (module M)
    (Some (module M))
    list
;;

[%%template
[@@@kind.default k = (value, float64, bits32, bits64, word)]

let require_allocation_does_not_exceed_local_private
  ?(cr = CR.CR)
  ?hide_positions
  ?(print_limit = 1_000)
  allocation_limit
  ?(here = Stdlib.Lexing.dummy_pos)
  f
  =
  let ( x
      , { Gc.For_testing.Allocation_report.major_words_allocated; minor_words_allocated }
      , allocs )
    =
    (Gc.For_testing.measure_and_log_allocation_local [@kind k]) f
  in
  let allocs = [%globalize: Gc.For_testing.Allocation_log.t list] allocs in
  require
    ~here
    ~cr
    ?hide_positions
    (Allocation_limit.is_ok
       allocation_limit
       ~major_words_allocated
       ~minor_words_allocated)
    ~if_false_then_print_s:
      (lazy
        (let minor_words_allocated, major_words_allocated =
           if CR.hide_unstable_output cr
           then None, None
           else if major_words_allocated > 0
                   || Allocation_limit.show_major_words allocation_limit
           then Some minor_words_allocated, Some major_words_allocated
           else Some minor_words_allocated, None
         in
         if not (CR.hide_unstable_output cr)
         then (
           let allocs =
             if List.length allocs > print_limit
             then (
               Printf.printf "Cutting off list of allocations after %d\n" print_limit;
               List.take allocs print_limit)
             else allocs
           in
           List.iter allocs ~f:(fun { size_in_words; is_major; backtrace } ->
             Printf.printf
               "Allocation of %d %s words occurred at:\n%s\n"
               size_in_words
               (if is_major then "major" else "minor")
               backtrace));
         [%message
           "allocation exceeded limit"
             (allocation_limit : Allocation_limit.t)
             (minor_words_allocated : (int option[@sexp.option]))
             (major_words_allocated : (int option[@sexp.option]))]));
  x
;;

let require_allocation_does_not_exceed_local
  ?cr
  ?print_limit
  ?hide_positions
  allocation_limit
  ?(here = Stdlib.Lexing.dummy_pos)
  f
  =
  (require_allocation_does_not_exceed_local_private [@kind k])
    ?cr
    ?print_limit
    ?hide_positions
    allocation_limit
    ~here
    f
;;

let require_no_allocation_local
  ?cr
  ?print_limit
  ?hide_positions
  ?(here = Stdlib.Lexing.dummy_pos)
  f
  =
  (require_allocation_does_not_exceed_local [@kind k])
    ?cr
    ?print_limit
    ?hide_positions
    (Minor_words 0)
    ~here
    f
;;

type 'a global = { global : 'a } [@@unboxed]

let require_allocation_does_not_exceed
  ?cr
  ?print_limit
  ?hide_positions
  limit
  ?(here = Stdlib.Lexing.dummy_pos)
  f
  =
  let { global } =
    (require_allocation_does_not_exceed_local [@kind k])
      ?cr
      ?print_limit
      ?hide_positions
      limit
      ~here
      (fun () -> { global = f () })
  in
  global
;;

let require_no_allocation
  ?cr
  ?print_limit
  ?hide_positions
  ?(here = Stdlib.Lexing.dummy_pos)
  f
  =
  let { global } =
    (require_no_allocation_local [@kind k])
      ?cr
      ?print_limit
      ?hide_positions
      ~here
      (fun () -> { global = f () })
  in
  global
;;]

let print_and_check_comparable_sexps
  (type a)
  ?cr
  ?hide_positions
  ?(here = Stdlib.Lexing.dummy_pos)
  (module M : With_comparable with type t = a)
  list
  =
  let set = Set.of_list (module M) list in
  let set_sexp = [%sexp (set : M.Set.t)] in
  print_s [%message "Set" ~_:(set_sexp : Sexp.t)];
  let sorted_list_sexp = [%sexp (List.sort list ~compare:M.compare : M.t list)] in
  require
    ?cr
    ?hide_positions
    ~here
    (Sexp.equal set_sexp sorted_list_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "set sexp does not match sorted list sexp"
            (set_sexp : Sexp.t)
            (sorted_list_sexp : Sexp.t)]);
  let alist = List.mapi list ~f:(fun i x -> x, i) in
  let map = Map.of_alist_exn (module M) alist in
  let map_sexp = [%sexp (map : int M.Map.t)] in
  print_s [%message "Map" ~_:(map_sexp : Sexp.t)];
  let sorted_alist_sexp =
    [%sexp
      (List.sort alist ~compare:(fun (x, _) (y, _) -> M.compare x y) : (M.t * int) list)]
  in
  require
    ?cr
    ?hide_positions
    ~here
    (Sexp.equal map_sexp sorted_alist_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "map sexp does not match sorted alist sexp"
            (map_sexp : Sexp.t)
            (sorted_alist_sexp : Sexp.t)])
;;

let print_and_check_hashable_sexps
  (type a)
  ?cr
  ?hide_positions
  ?(here = Stdlib.Lexing.dummy_pos)
  (module M : With_hashable with type t = a)
  list
  =
  let hash_set = Hash_set.of_list (module M) list in
  let hash_set_sexp = [%sexp (hash_set : M.Hash_set.t)] in
  print_s [%message "Hash_set" ~_:(hash_set_sexp : Sexp.t)];
  let sorted_list_sexp = [%sexp (List.sort list ~compare:M.compare : M.t list)] in
  require
    ?cr
    ?hide_positions
    ~here
    (Sexp.equal hash_set_sexp sorted_list_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "hash_set sexp does not match sorted list sexp"
            (hash_set_sexp : Sexp.t)
            (sorted_list_sexp : Sexp.t)]);
  let alist = List.mapi list ~f:(fun i x -> x, i) in
  let table = Hashtbl.of_alist_exn (module M) alist in
  let table_sexp = [%sexp (table : int M.Table.t)] in
  print_s [%message "Table" ~_:(table_sexp : Sexp.t)];
  let sorted_alist_sexp =
    [%sexp
      (List.sort alist ~compare:(fun (x, _) (y, _) -> M.compare x y) : (M.t * int) list)]
  in
  require
    ?cr
    ?hide_positions
    ~here
    (Sexp.equal table_sexp sorted_alist_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "table sexp does not match sorted alist sexp"
            (table_sexp : Sexp.t)
            (sorted_alist_sexp : Sexp.t)])
;;

let print_and_check_container_sexps
  (type a)
  ?cr
  ?hide_positions
  ?(here = Stdlib.Lexing.dummy_pos)
  m
  list
  =
  let (module M : With_containers with type t = a) = m in
  print_and_check_comparable_sexps ?cr ?hide_positions ~here (module M) list;
  print_and_check_hashable_sexps ?cr ?hide_positions ~here (module M) list
;;

let remove_time_spans =
  let span_regex =
    lazy
      (let sign = Re.set "-+" in
       let part =
         let integer = Re.rep1 Re.digit in
         let decimal = Re.opt (Re.seq [ Re.char '.'; Re.rep1 Re.digit ]) in
         let suffixes = List.map ~f:Re.str [ "d"; "h"; "m"; "s"; "ms"; "us"; "ns" ] in
         Re.seq [ integer; decimal; Re.alt suffixes ]
       in
       Re.compile (Re.seq [ Re.opt sign; Re.word (Re.rep1 part) ]))
  in
  fun string -> Re.replace_string (force span_regex) ~by:"SPAN" string
;;

module Expect_test_helpers_core_private = struct
  let require_allocation_does_not_exceed_local =
    require_allocation_does_not_exceed_local_private
  ;;
end
