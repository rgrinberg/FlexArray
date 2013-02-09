open OUnit

let braun_fix =
  let open Braun in
  "test braun" >:::
    [
      "leaf" >:: (fun () ->
        assert_equal (get (leaf "foo") 1) "foo"
      );
      "delete" >:: (fun () ->
        assert_equal empty (delete (leaf "foo") 1)
      );
      "init_seq" >:: begin fun () ->
        let br = create ~len:5 10 in
        List.iter (fun i -> assert_equal (get br i) 10) [1;2;3;4;5];
      end;
      "to_list" >:: begin fun () ->
        let br = create ~len:5 10 in
        assert_equal (to_list br) [10;10;10;10;10]
      end
    ] 
let fa_fix =
  let open FlexArray      in 
  let a = [|0;1;2;3;4;5|] in 
  let b = from_array a    in 
  "test flexible arrays" >:::
    [
      "create" >:: (fun () ->
        assert_equal (length empty) 0;
      );
      "snoc" >:: (fun () ->
        let arr = [|4;5;6|] in
        let fa = from_array arr in
        assert_equal arr (Array.map (fun i -> get fa i) [|0;1;2|])
      );
      "remove_last" >:: (fun () ->
        let orig    = from_array [|1;2;3|] in
        let removed = from_array [|1;2|] in
        assert_equal removed (remove_last orig)
      );
      "tail" >:: (fun () ->
        let arr = from_array [|1;2;3;4;5|] in
        assert_equal arr (tail b)
      );
      "cons" >:: (fun () ->
        let arr = from_array [|6;0;1;2;3;4;5|] in
        assert_equal arr (cons b 6)
      );
      "to_array" >:: (fun () ->
        let arr = [|1;2;3|] in
        let orig = from_array arr in
        assert_equal arr (to_array orig)
      );
      "from_array" >:: (fun () ->
        (*todo: this isn't a real test*)
        let arr = [|1;2;3|] in
        assert_equal (to_array (from_array arr)) arr;
      );
      "get" >:: (fun () ->
        assert_equal (get b 3) (a.(3));
      );
      "set" >:: (fun () ->
        assert_equal (get (set b 3 100) 3) 100;
      );
      "update" >:: begin fun () ->
        assert_equal (update b 3 ~f:(fun _ -> 111)) (set b 3 111);
      end;
      "to_list" >:: begin fun () ->
        let l = [1;2;3;4] in
        assert_equal (to_list (from_list l)) l
      end;
      "map" >:: begin fun () ->
        let l1 = [1;2;3;4;5] in
        let l2 = [-1;-2;-3;-4;-5] in
        let mapped = map (from_list l1) ~f:(fun x -> x * -1) in
        assert_equal (from_list l2) mapped;
      end;
      "iter_orderless" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let real_sum = List.fold_left (+) 0 l in
        let sum = ref 0 in
        iter_orderless (from_list l) ~f:(fun x -> sum := (!sum) + x);
        assert_equal (!sum) real_sum ~printer:string_of_int
      end;
      "iter" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let i = ref 1 in
        iter (from_list l) ~f:(fun x -> assert_equal (!i) x; incr i)
      end;
      "iter_reverse" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let i = ref 5 in
        iter_reverse (from_list l) ~f:(
          fun x -> assert_equal (!i) x ~printer:string_of_int ; decr i)
      end;
      "fold_left" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let f1 = List.fold_left (-) 0 l in
        let f2 = fold_left (from_list l) ~init:0 ~f:(-) in
        assert_equal f1 f2 ~printer:string_of_int
      end;
      "fold_right" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let f1 = List.fold_right (-) l 0 in
        let f2 = fold_right (from_list l) ~init:0 ~f:(-) in
        assert_equal f1 f2 ~printer:string_of_int
      end
    ]

let _ = run_test_tt ~verbose:true braun_fix
let _ = run_test_tt ~verbose:true fa_fix




