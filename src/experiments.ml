(* partie 3 *)

open RandomGenerator
open Compressor
open Constructor
open Unix
open Obj


let dtime f x = 
    let t0 = Unix. gettimeofday() in
    ignore (f x);
    let tf = Unix. gettimeofday() in
    tf -. t0   ;;

let get_memory_size n= 
    let sizeof (x: 'a) = Obj.reachable_words (Obj.repr x) in
    let p = RandomGenerator.gen_permutation2 1 n in
    let b = Constructor.construct p in
    let a = sizeof b in
    Printf.eprintf "%d %d\n" n a;;

let get_memory_compressed_size n= 
    let sizeof (x: 'a) = Obj.reachable_words (Obj.repr x) in
    let p = RandomGenerator.gen_permutation2 1 n in
    let b = Constructor.construct p in
    let ct = Compressor.compress b in
    let a = sizeof ct in
    Printf.eprintf "%d %d\n" n a;;

let get_avg_time n =
    let rec sum n i = 
        let p = RandomGenerator.gen_permutation n in
        let b = Constructor.construct p in
        let _ = Random.self_init () in
        let r = Random.int (n-1) in
        let t = dtime (Constructor.search b) r in
        if i = 4 then t else t +. (sum n (i+1))
    in let a = (sum n 0) /. 5.0 in
    Printf.eprintf "%d %f\n" n a;;

let get_compressed_avg_time n =
    let rec sum n i = 
        let p = RandomGenerator.gen_permutation2 1 n in
        let b = Constructor.construct p in
        let ct = Compressor.compress b in
        let _ = Random.self_init () in
        let r = 1 + Random.int (n-1) in
        let t = dtime (Compressor.search ct) r in
        if i = 4 then t else t +. (sum n (i+1))
    in let a = (sum n 0) /. 5.0 in
    Printf.eprintf "%d %f\n" n a;;

let test () =  
    begin
        let nvalues = [500; 1000; 2000; 3000; 4000; 5000; 6000; 7000; 8000; 9000; 10000; 11000; 12000; 13000; 14000; 15000; 16000; 17000 ;18000; 19000; 20000; 21000; 22000; 23000; 24000; 25000; 26000; 27000; 28000; 29000; 30000; 31000; 32000; 33000;34000; 35000] in 

        List.iter get_memory_size nvalues;
        List.iter get_memory_compressed_size nvalues;

        List.iter get_avg_time nvalues ;
        List.iter get_compressed_avg_time nvalues ;
    end
