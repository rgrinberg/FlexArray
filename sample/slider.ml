(* Example of using functional arrays to solve the "sliding block" puzzle *)
open Core.Std

module FA = FlexArray
module FA2 = FlexArray.F2D

type move = Left | Right | Up | Down

let random_state = Random.State.make_self_init ()

let string_of_move = function
  | Left  -> "left"
  | Right -> "right"
  | Up    -> "up"
  | Down  -> "down"

module Puzzle = struct
  type puzzle = {
    mat : int FA.t FA.t;
    zero : int * int; (* position of the zero element *) }
  type state = {
    puzzle : puzzle;
    moves : move list; } (* first move is last in the list *)

  (* not using core's hashtbl because don't want dependency on sexplib *)
  (* TODO : it's possible to record states instead of puzzles so that we 
   * could maybe do some crude moves length optimization for free *)

  module Hashtbl = Caml.Hashtbl.Make (struct
    type t = puzzle
    let equal = (=)
    let hash = Hashtbl.hash
  end)
end

open Puzzle

let square ~dim arr = 
  let mat = Array.make_matrix arr.(0) ~dimx:dim ~dimy:dim in
  for i = 0 to dim-1 do
    for j = 0 to dim-1 do
      mat.(i).(j) <- arr.((i * dim) + j);
    done
  done; mat

let identity ~dim = 
  let soln = Array.init ~f:succ (dim * dim) in
  soln.(pred (dim * dim)) <- 0;
  FA2.of_2d_array (square ~dim soln)

let puzzle_id ~dim = 
  let z = pred dim in { mat=(identity ~dim); zero=(z,z) }

(* actually this is the norm sqaured. but it makes no difference *)
let norm t = 
  let i = ref 0 in
  FA2.iter t ~f:(fun x -> i := (!i) + abs(x)); !i

let subtract t1 t2 = 
  FA.map2_exn t1 t2 ~f:(fun a b -> FA.map2_exn a b ~f:(fun x y -> x - y))

(* hashing for fast identity comparison does not work *)
let make_solution_checker ~dim = 
  let soln = identity ~dim in
  let h = Caml.Hashtbl.hash soln in
  begin fun {mat ; _} ->
    if h = Caml.Hashtbl.hash mat then soln = mat
    else false
  end

let zero_last arr = 
  let zero_posn = fst (Array.findi_exn arr ~f:(fun _ x -> x = 0)) in
  Array.swap arr zero_posn (Array.length arr - 1)

let new_puzzle_mat mat = 
  let (zx, zy) = FA2.dimensions mat in
  { puzzle={ mat; zero=(zx-1,zy-1) }; moves=[] }

let create_random_puzzle ~dim = 
  let rnd = Array.init ~f:(fun i -> i) (dim * dim) in
  Array.permute ~random_state rnd;
  zero_last rnd;
  let mat = square ~dim rnd in
  { mat=(FA2.of_2d_array mat); zero=(pred dim, pred dim) }

(** Calculates the position of the zero after a new move *)
let new_zero ~zero:(i,j) = function
  | Left  -> (i-1, j)
  | Right -> (i+1, j)
  | Up    -> (i, j-1)
  | Down  -> (i, j+1)

let opt_of_bool v b = if b then Some v else None

let valid_moves { mat; zero } = 
  [Left; Right; Up; Down]
  |> List.filter_map ~f:(fun p ->
    let nz = new_zero ~zero p in
    opt_of_bool p (FA2.inbounds mat nz))

let make_move { mat ; zero } move = 
  let new_zero = new_zero ~zero move in
  { mat=(FA2.swap mat zero new_zero); zero=new_zero }

let reverse_moves { puzzle ; moves } = { puzzle ; moves=(List.rev moves) }

exception Found_Soln of Puzzle.state
(* pass in a queue for collection for bfs and a stack for dfs *)
let xfs ~collection ~cache ~termination ~next ~start = 
  collection#add start;
  let rec loop () =
    match collection#remove with
    | None -> None
    | Some e when cache#visited e -> loop ()
    | Some e -> begin
      (if termination e then raise (Found_Soln e));
      ignore (cache#add_visit e);
      let frontier = next e in
      List.iter frontier ~f:(collection#add);
      loop ()
    end
  in try loop () with Found_Soln n -> Some (reverse_moves n)


module Collections : sig
  (* need this b/c we are not using every defined collection hence OCaml
   * will complain because it can't infer the type without usage... *)
  type t = < add : Puzzle.state -> unit; remove : Puzzle.state option >
  val queue : t
  val stack : t
  val priority_queue : id:Puzzle.state -> t
end = struct
  type t = < add : Puzzle.state -> unit; remove : Puzzle.state option >
  let queue = object
    val q = Queue.create ()
    method add = Queue.enqueue q
    method remove = Queue.dequeue q
  end

  let stack = object
    val q = Stack.create ()
    method add = Stack.push q
    method remove = Stack.pop q
  end

  let priority_queue ~id = 
    let id = id.puzzle.mat in object
      val q = Heap.create (fun {puzzle={mat=x;_};_ } { puzzle={mat=y;_};_ } ->
        compare (norm (subtract id x)) (norm (subtract id y)))
      method add e = (Heap.push q e) |> ignore
      method remove = Heap.pop q
  end
end

let array2d_of_puzzle { mat ; _ } = FA2.to_2d_array mat

let print_puzzle { mat ; _ } = 
  let arr = FA2.to_2d_array mat in
  let s = Array.sexp_of_t (Array.sexp_of_t Int.sexp_of_t) arr in
  print_endline (Sexp.to_string s)


let apply_moves puzzle moves = 
  let rec loop puzzle = function
    | [] -> puzzle
    | move::moves -> loop (make_move puzzle move) moves
  in loop puzzle moves

let verify ~id puzzle moves = (apply_moves puzzle moves) = id

let solve puzzle = 
  let dim = FA.length puzzle.mat in
  let id = identity ~dim in
  let termination = 
    let soln = make_solution_checker ~dim in
    (fun {puzzle; _ } -> soln puzzle ) in
  let collection = Collections.priority_queue ~id:(new_puzzle_mat id) in
  let cache = object
    val c = Hashtbl.create 10000
    method add_visit { puzzle ; _ } = Hashtbl.add c puzzle ()
    method visited { puzzle ; _ } = Hashtbl.mem c puzzle
  end in
  let next { puzzle ; moves } = 
    valid_moves puzzle 
    |> List.map ~f:(fun m -> { puzzle=(make_move puzzle m); moves=(m::moves) })
    (* don't need any of the following if we are using a PQ for ~collection *)
    (* we sort by norm from the solution, maybe it pays off for its cost? *)
    (*|> List.sort ~cmp:(fun { puzzle={mat=x;_};_ } { puzzle={mat=y;_};_ } ->*)
        (*compare (norm (subtract id x)) (norm (subtract id y)))*)
    (* "schwartzian" transform!!! *)
    (*|> List.map ~f:(fun p -> (norm (subtract id p.puzzle.mat), p))*)
    (*|> List.sort ~cmp:(fun (x, _) (y, _) -> compare x y)*)
    (*|> List.map ~f:snd*)
  in xfs ~collection ~cache ~termination ~next ~start:({puzzle; moves=[]})

let print_moves moves = 
  moves |> List.iter ~f:(fun m -> Printf.printf "%s\n" (string_of_move m))

let () = [2;3;4;5;6] |> List.iter ~f:(fun dim ->
  let random_puzzle = create_random_puzzle ~dim in
  print_endline "Attempting to solve:";
  print_puzzle random_puzzle;
  print_endline "-----------------------------------------";
  let soln = solve random_puzzle in
  match soln with
  | None -> print_endline "no solution was found"
  | Some { moves ; _ } -> begin
    let id = puzzle_id ~dim in
    if verify ~id random_puzzle moves 
    then begin 
      print_endline "{{{"; (print_moves moves); print_endline "}}}"
    end
    else begin
      (* print wrong solution to maybe diagnose *)
      print_endline "Solution doesn't work. End result is:";
      print_puzzle (apply_moves random_puzzle moves)
    end
  end;
  print_endline "-----------------------------------------";)
