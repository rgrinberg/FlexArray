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
      "create" >:: begin fun () ->
        let br = create ~len:5 10 in
        List.iter (fun i -> assert_equal (get br i) 10) [1;2;3;4;5];
      end;
      "to_list" >:: begin fun () ->
        let br = create ~len:5 10 in
        assert_equal (to_list br) [10;10;10;10;10]
      end
    ] 

let fa2d_fix = 
  let open FlexArray.F2D in
  "test 2d convenience functions" >:::
    [
      "of_2d_array" >:: begin fun () ->
        let (dimx, dimy) = (3, 3) in
        let arr = ArrayLabels.make_matrix ~dimx ~dimy 10 in
        let fa = of_2d_array arr in
        for i = 0 to dimx-1 do
          for j = 0 to dimy-2 do
            assert_equal 10 (get fa (i,j)) ~printer:string_of_int
          done
        done
      end;
      "to_2d_array" >:: begin fun () ->
        let arr = ArrayLabels.make_matrix ~dimx:4 ~dimy:4 10 in
        assert_equal (to_2d_array (of_2d_array arr)) arr
      end;
      "swap" >:: begin fun () ->
        let arr = ArrayLabels.init 6 ~f:(fun i -> 
          ArrayLabels.init 6 ~f:(fun j -> (i,j))) in
        let fa = of_2d_array arr in
        let fa2 = swap fa (1,2) (3,4) in
        arr.(1).(2) <- (3,4);
        arr.(3).(4) <- (1,2);
        assert_equal (of_2d_array arr) fa2
      end
    ]

let fa_fix =
  let open FlexArray      in 
  let a = [|0;1;2;3;4;5|] in 
  let b = of_array a    in 
  "test flexible arrays" >:::
    [
      "create" >:: (fun () ->
        assert_equal (length empty) 0;
      );
      "snoc" >:: (fun () ->
        let arr = [|4;5;6|] in
        let fa = of_array arr in
        assert_equal arr (Array.map (fun i -> get fa i) [|0;1;2|])
      );
      "remove_last" >:: (fun () ->
        let orig    = of_array [|1;2;3|] in
        let removed = of_array [|1;2|] in
        assert_equal removed (remove_last orig)
      );
      "tail" >:: (fun () ->
        let arr = of_array [|1;2;3;4;5|] in
        assert_equal arr (tail b)
      );
      "cons" >:: (fun () ->
        let arr = of_array [|6;0;1;2;3;4;5|] in
        assert_equal arr (cons b 6)
      );
      "to_array" >:: (fun () ->
        let arr = [|1;2;3|] in
        let orig = of_array arr in
        assert_equal arr (to_array orig)
      );
      "of_array" >:: (fun () ->
        (*todo: this isn't a real test*)
        let arr = [|1;2;3|] in
        assert_equal (to_array (of_array arr)) arr;
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
        assert_equal (to_list (of_list l)) l
      end;
      "map" >:: begin fun () ->
        let l1 = [1;2;3;4;5] in
        let l2 = [-1;-2;-3;-4;-5] in
        let mapped = map (of_list l1) ~f:(fun x -> x * -1) in
        assert_equal (of_list l2) mapped;
      end;
      "iter_orderless" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let real_sum = List.fold_left (+) 0 l in
        let sum = ref 0 in
        iter_orderless (of_list l) ~f:(fun x -> sum := (!sum) + x);
        assert_equal (!sum) real_sum ~printer:string_of_int
      end;
      "iter" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let i = ref 1 in
        iter (of_list l) ~f:(fun x -> assert_equal (!i) x; incr i)
      end;
      "iter_reverse" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let i = ref 5 in
        iter_reverse (of_list l) ~f:(
          fun x -> assert_equal (!i) x ~printer:string_of_int ; decr i)
      end;
      "fold_left" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let f1 = List.fold_left (-) 0 l in
        let f2 = fold_left (of_list l) ~init:0 ~f:(-) in
        assert_equal f1 f2 ~printer:string_of_int
      end;
      "fold_right" >:: begin fun () ->
        let l = [1;2;3;4;5] in
        let f1 = List.fold_right (-) l 0 in
        let f2 = fold_right (of_list l) ~init:0 ~f:(-) in
        assert_equal f1 f2 ~printer:string_of_int
      end;
      "create"  >:: begin fun () ->
        let fa1 = of_list [4;4;4] in
        let fa2 = create ~len:3 4 in
        assert_equal fa1 fa2
      end;
      "init" >:: begin fun () ->
        let fa1 = of_list [0;2;4;6] in
        let fa2 = init ~len:4 ~f:(fun x -> x * 2) in
        assert_equal fa1 fa2
      end;
      "swap" >:: begin fun () ->
        let f1 = of_list [1;2;3] in
        let f2 = of_list [3;2;1] in
        assert_equal (swap f1 0 2) f2
      end;
    ]

let _ = run_test_tt ~verbose:true braun_fix
let _ = run_test_tt ~verbose:true fa_fix
let _ = run_test_tt ~verbose:true fa2d_fix




