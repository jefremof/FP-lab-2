module type S0 = sig
  type s [@@deriving sexp, eq, ord]
  type els [@@deriving ord]

  val empty : s
  val is_empty : s -> bool
  val head : s -> s * s
  val append : s -> s -> s
  val split : els -> s
  val join : s -> els
end
