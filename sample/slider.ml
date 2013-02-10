(* Example of using functional arrays to solve the "sliding block" puzzle *)
open Core.Std

module FA = FlexArray
module FA2 = FlexArray.F2D

type move = | Left | Right | Up | Down

type puzzle = {
  mat : int FA.t FA.t;
  zero : int * int; (* position of the zero element *) }

let square ~dim arr = 
  let mat = Array.make_matrix arr.(0) ~dimx:dim ~dimy:dim in
  for i = 0 to dim-1 do
    for j = 0 to dim-1 do
      mat.(i).(j) <- (i * dim) + j;
    done
  done; mat

let make_solution_checker ~dim = 
  let soln = Array.init ~f:succ (dim * dim) in
  soln.(pred (dim * dim)) <- 0;
  let soln = FA2.of_2d_array (square ~dim soln) in
  (fun {mat ; _} -> soln = mat)

let zero_last arr = 
  let zero_posn = fst (Array.findi_exn arr ~f:(fun _ x -> x = 0)) in
  Array.swap arr zero_posn (Array.length arr - 1)

let create_random_puzzle ~dim = 
  let rnd = Array.init ~f:(fun i -> i) (dim * dim) in
  Array.permute rnd; zero_last rnd;
  let mat = square ~dim rnd in
  { mat=(FA2.of_2d_array mat); zero=(pred dim, pred dim) }

let new_zero ~zero:(i,j) = function
  | Left  -> (i-1, j)
  | Right -> (i+1, j)
  | Up    -> (i, j+1)
  | Down  -> (i, j-1)

let opt_of_bool v b = if b then Some v else None

let valid_moves { mat; zero } = 
  [Left; Right; Up; Down]
  |! List.filter_map ~f:(fun p ->
    let nz = new_zero ~zero p in
    opt_of_bool p (FA2.inbounds mat nz))

let make_move { mat ; zero } move = 
  let new_zero = new_zero ~zero move in
  { mat=(FA2.swap mat zero new_zero); zero=new_zero }

let () = print_endline "_$testing$_"
