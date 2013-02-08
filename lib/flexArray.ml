module B = Braun
exception Subscript
exception Size

(* TODO *)
(* 1. remove bound checking? *)

type 'a t = 'a B.t * int

let empty = (B.Empty, 0)

let length (_, n) = n

let get (t, n) k = 
  if (0 <= k) && (k < n) then B.get t (k+1)
  else raise Subscript

let set (t, n) k value = 
  if (0 <= k) && (k < n) then ((B.set t (k+1) value), n)
  else raise Subscript

let cons (t, n) value = (B.cons t value, n+1)

let tail (t, n) =
  if n > 0 then (B.tail t, n-1)
  else raise Size

let snoc (t, n) value = (B.set t (n+1) value, n+1)

let remove_last (t, n) = 
  if n > 0 then (B.delete t n, n-1)
  else raise Size

let update (t, n) k ~f = 
  if (0 <= k) && (k < n) then ((B.update t (k+1) ~f), n)
  else raise Subscript

let to_list (t, n) = B.to_list t

(* inefficient versions *)

(*let from_array arr = Array.fold ~init:empty ~f:snoc arr*)
let from_array arr = Array.fold_left snoc empty arr

(*let from_list l = List.fold ~init:empty ~f:snoc l*)
let from_list l = List.fold_left snoc empty l

(*let to_array (t, n) = Array.init n (get (t,n))*)
let to_array (t, n) = Array.init n (get (t,n))
