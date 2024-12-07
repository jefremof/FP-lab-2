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

module MakePrefixTree (Dep : Prefixes.S0) : Bag with type elt = Dep.els

module type Mapper = sig
  type elt1
  type elt2
  type t1
  type t2

  val polymorphic_map : (elt1 * int -> elt2 * int) -> t1 -> t2
end

module MakeMapper (Source : Bag) (Target : Bag) :
  Mapper
    with type elt1 := Source.elt
     and type elt2 := Target.elt
     and type t1 := Source.t
     and type t2 := Target.t

module type Comparator = sig
  type elt [@@deriving ord]
  type t

  val compare : t -> t -> int
end

module MakeComparator (Kind : Bag) :
  Comparator with type elt := Kind.elt and type t := Kind.t
