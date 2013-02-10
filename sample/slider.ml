(* Example of using functional arrays to solve the "sliding block" puzzle *)
open Core.Std
module FA = FlexArray

type puzzle = {
  mat : int FA.t FA.t;
  zero : int * int; (* position of the zero element *) }

type move = Coord of int * int

let zero_last arr = 
  let zero_posn = fst (Array.findi_exn arr ~f:(fun _ x -> x = 0)) in
  Array.swap arr zero_posn (Array.length arr - 1)

let create_random_puzzle ~dim = 
  let mat = Array.make_matrix 0 ~dimx:dim ~dimy:dim in
  let rnd = Array.init ~f:(fun i -> i) (dim * dim) in
  Array.permute rnd; zero_last rnd;
  for i = 0 to dim-1 do
    for j = 0 to dim-1 do
      let linear = (i * dim) + j in
      mat.(i).(j) <- rnd.(linear);
    done;
  done; { mat=(FA.F2D.of_2d_array mat); zero=(pred dim, pred dim) }

let valid_moves { mat; zero } = 
  let (i,j) = zero in
  let (up, down, left, right) = ((i,j+1), (i,j-1), (i-1, j), (i+1,j)) in
  List.map [up; down; left; right] ~f:(fun dest -> dest)

let make_move { mat ; zero=(z1,z2) } (Coord (i,j)) = 
  let v = () in ()


let () = print_endline "testing"
