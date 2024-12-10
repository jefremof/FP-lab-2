open Fp_lab_2.Presets
open Fp_lab_2.Trie
open Core
open IntSet
module Comp = MakeComparator (IntSet)

let%test_unit "length" =
  let bag = from_list2 [ (1, 2); (2, 3) ] in
  let len = length bag in
  [%test_eq: int] len 5

let%test_unit "remove" =
  let bag = from_list2 [ (4, 5); (100, 200) ] in
  let removed = remove Int.max_value 4 bag in
  let expected = from_list2 [ (100, 200) ] in
  [%test_eq: int] (Comp.compare removed expected) 0

let%test_unit "merge" =
  let bag1 = from_list2 [ (1, 2); (2, 3) ] in
  let bag2 = from_list2 [ (1, 5); (5, 4) ] in
  let merged = merge bag1 bag2 in
  let expected = from_list2 [ (1, 7); (2, 3); (5, 4) ] in
  [%test_eq: int] (Comp.compare merged expected) 0

let%test_unit "conversion" =
  let list = [ (1, 2); (2, 3) ] in
  let bag = from_list2 list in
  [%test_eq: (int * int) list] list (to_list2 bag)
