type 'a t

exception Subscript

exception Size

val empty : 'a t

val length : 'a t -> int

val get : 'a t-> int -> 'a

val set : 'a t -> int -> 'a -> 'a t

val cons : 'a t -> 'a -> 'a t

val tail : 'a t -> 'a t

val snoc : 'a t -> 'a -> 'a t

val remove_last : 'a t -> 'a t

val update : 'a t -> int -> f:('a -> 'a) -> 'a t

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

val of_array : 'a array -> 'a t

val to_array : 'a t -> 'a array

val map : 'a t -> f:('a -> 'b) -> 'b t

(* throws Invalid_argument when arrays are of different size *)
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val iter : 'a t -> f:('a -> unit) -> unit

val iter_reverse : 'a t -> f:('a -> unit) -> unit

val iter_orderless : 'a t -> f:('a -> unit) -> unit

val fold_right : 'a t -> f:('a -> 'accum -> 'accum) -> init:'accum -> 'accum

val fold_left : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

val init : len:int -> f:(int -> 'a) -> 'a t

val create : len:int -> 'a -> 'a t

val swap : 'a t -> int -> int -> 'a t

val inbounds : 'a t -> int -> bool

module F2D : sig
  val get : 'a t t -> int * int -> 'a
  val set : 'a t t -> int * int -> 'a -> 'a t t
  val swap : 'a t t -> int * int -> int * int -> 'a t t
  val create : dimx:int -> dimy:int -> 'a -> 'a t t
  val init : dimx:int -> dimy:int -> f:(x:int -> y:int -> 'a) -> 'a t t
  val of_2d_array : 'a array array -> 'a t t
  val to_2d_array : 'a t t -> 'a array array
  val dimensions : 'a t t -> int * int
  val inbounds : 'a t t -> int * int -> bool
end
