open Core
open Utils

module type Bag = sig
  type t [@@deriving sexp]
  type elt [@@deriving ord]

  val empty : t
  val add : int -> elt -> t -> t
  val add2 : elt * int -> t -> t
  val remove : int -> elt -> t -> t
  val remove2 : elt * int -> t -> t
  val merge : t -> t -> t
  val count : elt -> t -> int
  val fold_left : t -> 'acc -> ('acc -> elt -> 'acc) -> 'acc
  val fold_left2 : t -> 'acc -> ('acc -> elt * int -> 'acc) -> 'acc
  val fold_right : t -> (elt -> 'acc -> 'acc) -> 'acc -> 'acc
  val fold_right2 : t -> (elt * int -> 'acc -> 'acc) -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val map2 : (elt * int -> elt * int) -> t -> t
  val filter : (elt * int -> bool) -> t -> t
  val to_list2 : t -> (elt * int) list
  val from_list2 : (elt * int) list -> t
end

module MakePrefixTree (Dep : Prefixes.S0) = struct
  type 'a tree = Root of (int * 'a tree list) | Node of ('a * 'a tree list)
  [@@deriving sexp]

  type 'a cell = 'a * int [@@deriving sexp]
  type t = Dep.s cell tree [@@deriving sexp]
  type elt = Dep.els [@@deriving ord]

  let empty = Root (0, [])

  module Nodes = struct
    let get_label = function Root _ -> Dep.empty | Node ((x, _), _) -> x
    let get_mult = function Root (n, _) -> n | Node ((_, n), _) -> n
    let get_subnodes = function Root (_, xs) -> xs | Node (_, xs) -> xs
    let as_tuple node = (get_label node, get_mult node, get_subnodes node)
    let equal x y = [%eq: Dep.s] (get_label x) (get_label y)

    let set (l, n, ns) = function
      | Node (_, _) -> Node ((l, n), ns)
      | Root (_, _) -> if Dep.is_empty l then Root (n, ns) else Node ((l, n), ns)

    let rec make_pos node =
      let label, mult, subnodes = as_tuple node in
      set (label, pos mult, List.map ~f:make_pos subnodes) node

    let sort =
      List.sort ~compare:(fun x y -> [%ord: Dep.s] (get_label x) (get_label y))
  end

  let rec merge tree1 tree2 =
    match (tree1, tree2) with
    | Root (0, []), _ -> Nodes.make_pos tree2
    | _, Root (0, []) -> Nodes.make_pos tree1
    | Root (n, xs), Root (m, ys) -> Root (pos (n + m), merge_subnodes xs ys)
    | Root (n, xs), (Node _ as y) -> Root (n, merge_subnodes xs [ y ])
    | (Node _ as x), Root (m, ys) -> Root (m, merge_subnodes ys [ x ])
    | (Node _ as x), (Node _ as y) ->
        if Nodes.equal x y then inner_node_absorb x y
        else Root (0, [ x; Nodes.make_pos y ])

  and merge_subnodes xs ys =
    let find_match x ys = List.find ys ~f:(Nodes.equal x) in
    let no_match xs y = Option.is_none (find_match y xs) in
    let merge_matches x = Option.fold (find_match x ys) ~init:x ~f:merge in
    List.map xs ~f:merge_matches
    @ (List.filter ys ~f:(no_match xs) |> List.map ~f:Nodes.make_pos)

  and inner_node_absorb x y =
    match (x, y) with
    | Node ((x, n), xs), Node ((_, m), ys) ->
        Node ((x, plus n m), merge_subnodes xs ys)
    | Root (n, xs), Root (m, ys) -> Root (plus n m, merge_subnodes xs ys)
    | _ -> failwith "Impossible situation"

  let inner_fold_left (type b) (x : t) (a : b) (f : b -> Dep.s * int -> b) : b =
    let rec aux (f : b -> Dep.s * int -> b) (prefixes : Dep.s) (acc : b)
        (tree : t) =
      let label, n, subnodes = Nodes.as_tuple tree in
      let value = Dep.append prefixes label in
      let rec_acc =
        match Nodes.sort subnodes with
        | [] -> acc
        | xs -> List.fold_left xs ~init:acc ~f:(aux f value)
      in
      f rec_acc (value, n)
    in
    aux f Dep.empty a x

  let fold_left2 (type b) (tree : t) (acc : b) (f : b -> elt * int -> b) : b =
    inner_fold_left tree acc (fun acc (x, n) -> f acc (Dep.join x, n))

  let fold_left (type b) (tree : t) (acc : b) (f : b -> elt -> b) : b =
    inner_fold_left tree acc (fun acc (x, _) -> f acc (Dep.join x))

  let inner_fold_right (type b) (tree : t) (acc : b) (f : Dep.s * int -> b -> b)
      : b =
    let rec aux (f : Dep.s * int -> b -> b) (prefixes : Dep.s) (tree : t)
        (acc : b) =
      let label, n, subnodes = Nodes.as_tuple tree in
      let value = Dep.append prefixes label in
      let rec_acc =
        match Nodes.sort subnodes with
        | [] -> acc
        | xs -> List.fold_right xs ~f:(aux f value) ~init:acc
      in
      f (value, n) rec_acc
    in
    aux f Dep.empty tree acc

  let fold_right (type b) (tree : t) (f : elt -> b -> b) (acc : b) : b =
    inner_fold_right tree acc (fun (x, _) -> f (Dep.join x))

  let fold_right2 (type b) (tree : t) (f : elt * int -> b -> b) (acc : b) : b =
    inner_fold_right tree acc (fun (x, n) -> f (Dep.join x, n))

  let count (key : elt) (tree : t) : int =
    let prefixes = Dep.split key in
    inner_fold_left tree 0 (fun acc (x, n) ->
        if [%eq: Dep.s] x prefixes then n else acc)

  let inner_insert n (key : elt) (tree : t) : t =
    let rec chain (x, xs) n : t =
      let m = if Dep.is_empty xs then n else 0
      and further = if Dep.is_empty xs then [] else [ chain (Dep.head xs) n ] in
      Node ((x, m), further)
    in
    let x, xs = Dep.head (Dep.split key) in
    let chained = chain (x, xs) n in
    let node = if Dep.is_empty x then Root (n, []) else chained in
    merge tree node

  let add2 (key, n) (tree : t) =
    assert (n >= 0);
    inner_insert n key tree

  let add n (key : elt) (tree : t) = add2 (key, n) tree

  let remove2 (key, n) (tree : t) =
    assert (n >= 0);
    inner_insert (-n) key tree

  let remove n (key : elt) (tree : t) = remove2 (key, n) tree

  let to_list2 tree =
    let aux acc (x, n) = if n > 0 then (Dep.join x, n) :: acc else acc in
    inner_fold_left tree [] aux |> List.sort ~compare:[%ord: elt * int]

  let from_list2 xs = List.fold_left xs ~init:empty ~f:(Fn.flip add2)
  let map2 f tree = fold_left2 tree empty (fun t ln -> add2 (f ln) t)
  let map f = map2 (fun (l, n) -> (f l, n))

  let filter f =
    let aux ln = if f ln then ln else (fst ln, 0) in
    map2 aux
end

module type Mapper = sig
  type elt1
  type elt2
  type t1
  type t2

  val polymorphic_map : (elt1 * int -> elt2 * int) -> t1 -> t2
end

module MakeMapper (Source : Bag) (Target : Bag) = struct
  type elt1 = Source.elt
  type elt2 = Target.elt
  type t1 = Source.t
  type t2 = Target.t

  let polymorphic_map (f : elt1 * int -> elt2 * int) (set : t1) : t2 =
    Source.fold_left2 set Target.empty (fun t ln -> Target.add2 (f ln) t)
end

module type Comparator = sig
  type elt [@@deriving ord]
  type t

  val compare : t -> t -> int
end

module MakeComparator (Kind : Bag) = struct
  type elt = Kind.elt [@@deriving ord]
  type t = Kind.t

  let compare (x : t) (y : t) =
    let merged = Kind.merge x y in
    let entries_ord acc e =
      if acc <> 0 then acc else Int.compare (Kind.count e x) (Kind.count e y)
    in
    Kind.fold_left merged 0 entries_ord
end
