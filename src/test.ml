open Constructor ;;
open RandomGenerator ;;
open Compressor ;;
open Experiments ;;

let sizeof (x: 'a) = Obj.reachable_words (Obj.repr x)

let rec print_list = function
    | [] -> print_string("")
    | x :: xs -> (print_string((string_of_int x) ^ " "); print_list xs)


let main () = 
    (* let _ = print_endline("Enter the size : ") in
    let n = read_int () in *)
    begin
        Experiments.test(); 
        (* let perm = gen_permutation2 1 n in
        print_list perm;
        print_newline () ;
        let bt = construct perm in
        let ct = compress bt in
        let _ = Random.self_init () in
        let r = Random.int (2*n) in *)
        (* print_endline("r = "^ (string_of_int r));
        print_endline(string_of_bool (search ct r)); *)
    end ;;

main () ;;
