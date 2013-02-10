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

let to_list (t, _) = B.to_list t

(* inefficient versions *)

let from_array arr = Array.fold_left snoc empty arr

let from_list l = List.fold_left snoc empty l

let to_array (t, n) = Array.init n (get (t,n))

let map (t, n) ~f = (B.map t ~f, n)

(* TODO : this is a slow and shitty version for now *)
let iter (t,n) ~f =
  for i = 1 to n do f (B.get t i); done

let iter_reverse (t,n) ~f = 
  for i = n downto 1 do f (B.get t i); done

let iter_orderless (t, _) ~f = B.iter_orderless t ~f

let fold_left t ~init ~f = 
  let accum = ref init in
  iter t ~f:(fun x ->
    accum := f (!accum) x
  ); !accum

let fold_right t ~f ~init = 
  let accum = ref init in
  iter_reverse t ~f:(fun x ->
    accum := f x (!accum)
  ); !accum

let from_2d_array arr =
  Array.fold_left (fun acc x -> snoc acc (from_array x)) empty arr
