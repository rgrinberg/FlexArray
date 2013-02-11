type 'a t = 
  | Empty
  | Branch of 'a t * 'a * 'a t

exception Subscript
(*raised whenever we delete from an empty array*)
exception Size

let empty = Empty

let leaf v = Branch (Empty, v, Empty)

let rec get t i = 
  match t with
  | Empty -> raise Subscript
  | Branch (left, x, right) ->
      if i = 1 then x
      else 
        if i mod 2 = 0
        then get left (i / 2) 
        else get right (i / 2)
        
let rec set t i value =
  match t with
  | Empty -> if i = 1 then (leaf value) else raise Subscript
  | Branch(left, x, right) ->
      if i = 1 then Branch(left, value, right)
      else
        if (i mod 2) = 0
        then Branch ( (set left (i / 2) value), x, right )
        else Branch ( left, x, (set  right (i / 2) value) )

let rec update t i ~f = 
  match t with
  | Empty -> raise (if i = 1 then Not_found else Subscript)
  | Branch(left, x, right) ->
      if i = 1 then Branch(left, (f x), right)
      else
        if (i mod 2) = 0
        then Branch ( (update left (i / 2) ~f), x, right )
        else Branch ( left, x, (update  right (i / 2) ~f) )

let rec delete t i =
  match t with
  | Empty -> raise Subscript
  | Branch(left, x, right) ->
      if i = 1 then Empty
      else
        if (i mod 2) = 0 
        then Branch ( (delete left (i / 2)), x, right )
        else Branch ( left, x, (delete right (i / 2)) )

let rec cons t a = 
  match t with
  | Empty -> leaf a
  | Branch(left, x, right) -> Branch((cons right x), a, left)

let rec create ~len:n a = 
  match n with
  | 0 -> Empty
  | _ -> Branch( create ~len:(n/2) a , a , create ~len:((n-1)/2) a )

let rec tail t =
  match t with
  | Empty -> raise Size
  | Branch(Empty, _, Empty) -> Empty
  | Branch( (Branch(_,v,_) as left) ,_ , right ) ->
      Branch(right, v, (tail left))
      
let rec interleave = function
  | xs, [] -> xs
  | x::xs, y::ys -> x::y::(interleave(xs, ys))
  
let rec to_list = function
  | Empty -> []
  | Branch (left, x, right) ->
      x::(interleave(to_list left, (to_list right)))

let rec map t ~f =
  match t with
  | Empty -> Empty
  | Branch (left, x, right) -> Branch (map left ~f, (f x), map right ~f)

let rec map2 t1 t2 ~f = 
  match t1, t2 with
  | Empty, Empty -> Empty
  | Branch (l1, x1, r1), Branch (l2, x2, r2) ->
      Branch (map2 l1 l2 ~f, (f x1 x2), map2 r1 r2 ~f)
  | _, _ -> raise (Invalid_argument "map2: different sizes of arrays")

let rec iter_orderless t ~f = 
  match t with
  | Empty -> ()
  | Branch (left, x, right) -> begin
      f x; iter_orderless left ~f; iter_orderless right ~f
  end
