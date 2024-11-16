type color =
  | Red
  | Black

type 'a t =
  | Leaf
  | Node of color * 'a * 'a t * 'a t

val empty : 'a t
val insert : 'a -> 'a t -> 'a t
val to_string : ('a -> string) -> 'a t -> int -> string
