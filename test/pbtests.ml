open Core
open Fp_lab_2.Trie
open Fp_lab_2.Utils
open Fp_lab_2.Presets
module Comp = MakeComparator (IntSet)
module Gen = Quickcheck.Generator

let gen_int = Int.gen_incl 1 Int.max_value
and gen_count = Int.gen_incl 0 Int.max_value

let compare = [%ord: int * int]

let gen_list =
  let gen_pair = Gen.both gen_int gen_int in
  let make_unique_int = make_unique_with_eq [%eq: int] in
  let enhance x = make_unique_int x |> List.sort ~compare in
  List.gen_non_empty gen_pair |> Gen.map ~f:enhance

let gen_bag = Gen.map ~f:IntSet.from_list2 gen_list

let gen_bag2 = Gen.tuple2 gen_bag gen_bag
and gen_bag3 = Gen.tuple3 gen_bag gen_bag gen_bag

let with_overflow f x = try f x |> Some with Overflow -> None
let with_overflow_d f x d = try f x with Overflow -> d

let test_bag_equal f g input =
  [%test_eq: int]
    (with_overflow_d (fun i -> Comp.compare (f i) (g i)) input 0)
    0

let%test_unit "Converting list to bag preserves elements" =
  Quickcheck.test ~sexp_of:[%sexp_of: (int * int) list] gen_list ~f:(fun xs ->
      [%test_eq: (int * int) list] xs (IntSet.from_list2 xs |> IntSet.to_list2))

let%test_unit "Length adequacy" =
  let f acc (_, n) = acc + n in
  let list_length xs = IntSet.to_list2 xs |> List.fold_left ~init:0 ~f in
  let check xs = [%test_eq: int] (IntSet.length xs) (list_length xs) in
  Quickcheck.test gen_bag ~f:check

let%test_unit "Comparison commutativity" =
  Quickcheck.test gen_bag2 ~f:(fun (x, y) ->
      [%test_eq: int option]
        (with_overflow (Fn.compose ( ~- ) (Comp.compare x)) y)
        (with_overflow (Comp.compare y) x))

let%test_unit "Monoid identity" =
  let merge x = IntSet.merge x IntSet.empty in
  let f = test_bag_equal merge Fn.id in
  Quickcheck.test gen_bag ~f

let%test_unit "Monoid Commutativity" =
  let merge_a (x, y) = IntSet.merge x y in
  let merge_b (x, y) = merge_a (y, x) in
  let f = test_bag_equal merge_a merge_b in
  Quickcheck.test gen_bag2 ~f

let%test_unit "Monoid Associativity" =
  let merge3_a (x, y, z) = IntSet.merge x (IntSet.merge y z)
  and merge3_b (x, y, z) = IntSet.merge (IntSet.merge x y) z in
  let f = test_bag_equal merge3_a merge3_b in
  Quickcheck.test gen_bag3 ~f

let gen_addition = Gen.tuple3 gen_bag gen_int gen_int

let%test_unit "Simple addition" =
  Quickcheck.test gen_addition ~f:(fun (xs, k, n) ->
      [%test_eq: int option]
        (with_overflow (fun x -> IntSet.add n k x |> IntSet.count k) xs)
        (with_overflow (plus (IntSet.count k xs)) n))

let%test_unit "Simple removal" =
  Quickcheck.test gen_addition ~f:(fun (xs, k, n) ->
      [%test_eq: int]
        (IntSet.remove n k xs |> IntSet.count k)
        (max (IntSet.count k xs - n) 0))

(* Polymorphic map testing
   Generates random functions (gen_int2_to_string2) and bags as input*)

module Mapping = MakeMapper (IntSet) (StringSet)

let gen_int2_to_string2 =
  let observe_pairs =
    Quickcheck.Observer.tuple2 Int.quickcheck_observer Int.quickcheck_observer
  in
  let gen_pair = Gen.both String.quickcheck_generator gen_count in
  Gen.fn observe_pairs gen_pair

let make_unique_result f a : (int * int) list =
  let eq = [%eq: string] in
  let aux xs x =
    if List.exists xs ~f:(fun y -> eq (fst (f x)) (fst (f y))) then xs
    else x :: xs
  in
  List.fold_left a ~init:[] ~f:aux

let gen_bag_and_fn =
  let gen_filtered_bag fn =
    let f xs = make_unique_result fn xs |> IntSet.from_list2 in
    Gen.map ~f gen_list
  in
  let zip fn = Gen.both (gen_filtered_bag fn) (Gen.return fn) in
  gen_int2_to_string2 |> Gen.bind ~f:zip

let check_map tree (fn : int * int -> string * int) mapped =
  let check_length =
    let expected =
      let sum acc pair = acc + (fn pair |> snd) in
      IntSet.fold_left2 tree 0 sum
    in
    StringSet.length mapped = expected
  and check_entries =
    let check_entry pair =
      let y, m = fn pair in
      StringSet.count y mapped = m
    in
    let all acc x = acc && check_entry x in
    IntSet.fold_left2 tree true all
  in
  check_length && check_entries

let%test_unit "Polymorphic map" =
  let f (xs, fn) =
    let check =
      let map = Mapping.map fn in
      let checked xs = map xs |> check_map xs fn in
      with_overflow_d checked xs true
    in
    [%test_result: bool] check ~expect:true
  in
  Quickcheck.test gen_bag_and_fn ~f
