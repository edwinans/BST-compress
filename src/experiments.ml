(* partie 3 *)

open RandomGenerator
open Compressor
open Constructor
open Unix
open Obj


let sizeof (x: 'a) = Obj.reachable_words (Obj.repr x)

let dtime f x = 
    let t0 = Unix. gettimeofday() in
    ignore (f x);
    let tf = Unix. gettimeofday() in
    tf -. t0   ;;

let get_memory_size n= 
    let p = RandomGenerator.gen_permutation2 1 n in
    let bt = Constructor.construct p in
    let ct = Compressor.compress bt in
    let bt_size = sizeof bt and ct_size = sizeof ct in
    Printf.eprintf "tree_size:%d | bt_memory:%d | ct_memory:%d\n" n bt_size ct_size;;

let get_time n =
    let sum1 = ref 0. and sum2 = ref 0. in
    let p = RandomGenerator.gen_permutation n in
    let bt = Constructor.construct p in
    let ct = Compressor.compress bt in
    let _ = Random.self_init () in
    let r = Random.int (n) in
    for i=0 to 1000 do
        let bt_time = dtime (Constructor.search bt) r and
        ct_time = dtime (Compressor.search ct) r in
        (sum1:= !sum1 +. bt_time; sum2:= !sum2 +. ct_time)
    done;
    Printf.eprintf "size:%d | bt_time:%f | ct_time:%f\n" n (!sum1) (!sum2) ;;

let test () =  
    begin
        let nvalues = [1;3;5;10;15;20;30;40;50;60;70;80;90;100;200;300;400;500;1000;2000;2500;3000;3500;4000;4500;5000;5200;5500;6000] in 

        List.iter get_memory_size nvalues;
        print_endline ("__________________________________________________");
        List.iter get_time nvalues ;
    end
