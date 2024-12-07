open Core

exception Overflow

let pos x = max x 0

let plus n m =
  if n >= 0 && m >= 0 && n + m < n then raise Overflow else pos (n + m)

let make_unique_with_eq eq a =
  let aux xs (x, n) =
    if List.exists xs ~f:(fun (y, _) -> eq x y) then xs else (x, n) :: xs
  in
  List.fold_left a ~init:[] ~f:aux

let digits_of n =
  let is_negative = n < 0 in
  let rec aux n acc = if n = 0 then acc else aux (n / 10) (acc @ [ n % 10 ]) in
  let digits = aux (abs n) [] in
  match digits with
  | [] -> [ 0 ]
  | x :: xs -> if is_negative then -x :: xs else x :: xs

let number_from_digits digits : int =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux ((acc * 10) + abs x) xs
  in
  match digits with
  | [] -> 0
  | x :: xs -> if x < 0 then -aux 0 xs else aux 0 digits
