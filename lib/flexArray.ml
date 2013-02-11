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

let of_array arr = Array.fold_left snoc empty arr

let of_list l = List.fold_left snoc empty l

let to_array (t, n) = Array.init n (get (t,n))

let map (t, n) ~f = (B.map t ~f, n)

let map2_exn (t1, n) (t2, m) ~f =
  if n = m then (B.map2 t1 t2 ~f, n)
  else raise (Invalid_argument "map2_exn: arrays of different size")

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


let create ~len a = (B.create ~len a, len)

(* this is really inefficient but will do for now *)
let init ~len ~f = 
  let arr = create ~len (f 0) in
  let rec loop t = function
    | 0 -> set t 0 (f 0)
    | n -> loop (set t n (f n)) (pred n)
  in loop arr (pred len)

let swap t i j =
  let e = get t i in
  let f = set t i (get t j) in
  set f j e

let inbounds t i = try (get t i); true with Subscript -> false

(* Helper functions for 2d array manipulations *)
module F2D = struct
  (* need this ugliness because we cannot reference functions in parent
   * module *)
  let get2 t (i, j) = get (get t i) j
  let set2 t (i, j) v = 
    let inner = get t i in
    set t i (set inner j v)

  (* NOTE : this function is not "safe" *)
  let dimensions t =
    let j = length (get t 0) in
    (length t, j)

  let get = get2
  let set = set2

  let inbounds t i = try (get t i); true with Subscript -> false

  let swap t i j =
    let e = get t i in
    let f = set t i (get t j) in
    set f j e

  let create ~dimx ~dimy a = 
    let inner = create ~len:dimy a in
    create ~len:dimx inner

  let init ~dimx ~dimy ~f =
    init ~len:dimx ~f:(fun x ->
      init ~len:dimy ~f:(fun y -> f ~x ~y))

  let of_2d_array arr =
    Array.fold_left (fun acc x -> snoc acc (of_array x)) empty arr

  let to_2d_array arr = 
    Array.map to_array (to_array arr)
end

