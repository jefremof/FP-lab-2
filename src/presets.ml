open Core
open Trie
open Utils

module IntSet = MakePrefixTree (struct
  type els = int [@@deriving ord]
  type s = int list [@@deriving sexp, eq, ord]

  let empty = []
  let is_empty = [%eq: s] empty
  let join n = number_from_digits (List.rev n)
  let head = function x :: xs -> (x :: [], xs) | [] -> ([], [])
  let append = ( @ )
  let split n = digits_of n
end)

module StringSet = MakePrefixTree (struct
  type s = char list [@@deriving sexp, eq, ord]
  type els = string [@@deriving ord]

  let empty = []
  let is_empty = [%eq: s] empty
  let join xs = Base.String.concat (Base.List.map xs ~f:(Base.String.make 1))
  let head = function x :: xs -> (x :: [], xs) | [] -> ([], [])
  let append = ( @ )
  let split x = Base.List.init (String.length x) ~f:(Base.String.get x)
end)
