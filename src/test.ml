open Constructor ;;
open RandomGenerator ;;
open Compressor ;;

let rec print_list = function
    | [] -> print_string("")
    | x :: xs -> (print_string((string_of_int x) ^ " "); print_list xs)

let main () = 
    let _ = print_endline("Enter the size : ") in
    let n = read_int () in
    let perm = gen_permutation n in let _ = print_list perm in
    let bt = construct (perm) in 
    let ct = compress bt in
    begin
        let _= Random.self_init () in
        let r = 1 + Random.int (n-1) in
        print_endline(string_of_bool (search ct r));
    end ;;

main () ;;
