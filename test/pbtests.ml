open Core
open Fp_lab_2.Trie
open Fp_lab_2.Utils
open Fp_lab_2.Presets
module Comp = MakeComparator (IntSet)

let compare = [%derive.ord: int * int]
let make_unique = make_unique_with_eq [%derive.eq: int]
let gen_int = Int.gen_incl 1 Int.max_value

let gen_list =
  let gen_pair = Quickcheck.Generator.both gen_int gen_int in
  let enhance x = make_unique x |> List.sort ~compare in
  List.gen_non_empty gen_pair |> Quickcheck.Generator.map ~f:enhance

let gen_bag = Quickcheck.Generator.map ~f:IntSet.from_list2 gen_list

let gen_bag2 = Quickcheck.Generator.tuple2 gen_bag gen_bag
and gen_bag3 = Quickcheck.Generator.tuple3 gen_bag gen_bag gen_bag

let gen_addition = Quickcheck.Generator.tuple3 gen_bag gen_int gen_int
let gen_removal = gen_addition
let with_overflow f x = try f x |> Some with Overflow -> None
let with_overflow_d f x d = try f x with Overflow -> d

let merge3_a (x, y, z) = IntSet.merge x (IntSet.merge y z)
and merge3_b (x, y, z) = IntSet.merge (IntSet.merge x y) z

let%test_unit "Converting list to bag preserves elements" =
  Quickcheck.test ~sexp_of:[%sexp_of: (int * int) list] gen_list ~f:(fun xs ->
      [%test_eq: (int * int) list] xs (IntSet.from_list2 xs |> IntSet.to_list2))

let%test_unit "Comparison commutativity" =
  Quickcheck.test gen_bag2 ~f:(fun (x, y) ->
      [%test_eq: int option]
        (with_overflow (Comp.compare x) y)
        (with_overflow (fun z -> -Comp.compare z x) y))

let%test_unit "Monoid identity" =
  Quickcheck.test gen_bag ~f:(fun xs ->
      [%test_eq: int]
        (with_overflow_d
           (fun x -> IntSet.merge x IntSet.empty |> Comp.compare x)
           xs 0)
        0)

let%test_unit "Monoid Commutativity" =
  Quickcheck.test gen_bag2 ~f:(fun inp ->
      [%test_eq: (int * int) list option]
        (with_overflow
           (fun (xs, ys) -> IntSet.merge xs ys |> IntSet.to_list2)
           inp)
        (with_overflow
           (fun (xs, ys) -> IntSet.merge ys xs |> IntSet.to_list2)
           inp))

let%test_unit "Monoid Associativity" =
  Quickcheck.test gen_bag3 ~f:(fun inp ->
      [%test_eq: (int * int) list option]
        (with_overflow (fun x -> merge3_a x |> IntSet.to_list2) inp)
        (with_overflow (fun x -> merge3_b x |> IntSet.to_list2) inp))

let%test_unit "Simple addition" =
  Quickcheck.test gen_addition ~f:(fun (xs, k, n) ->
      [%test_eq: int option]
        (with_overflow (fun x -> IntSet.add n k x |> IntSet.count k) xs)
        (with_overflow (plus (IntSet.count k xs)) n))

let%test_unit "Simple substraction" =
  Quickcheck.test gen_removal ~f:(fun (xs, k, n) ->
      [%test_eq: int]
        (IntSet.remove n k xs |> IntSet.count k)
        (max (IntSet.count k xs - n) 0))
