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

val from_list : 'a list -> 'a t

val from_array : 'a array -> 'a t

val to_array : 'a t -> 'a array

val map : 'a t -> f:('a -> 'b) -> 'b t

val iter : 'a t -> f:('a -> unit) -> unit

val iter_reverse : 'a t -> f:('a -> unit) -> unit

val iter_orderless : 'a t -> f:('a -> unit) -> unit

val fold_right : 'a t -> f:('a -> 'accum -> 'accum) -> init:'accum -> 'accum

val fold_left : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

val from_2d_array : 'a array array -> 'a t t
