open Constructor ;;
open RandomGenerator ;;
open Compressor ;;


let main () = 
    let _ = print_endline("Enter the size : ") in
    let n = read_int () in
    let bt = construct (gen_permutation2 1 n) in 
    let ct = compress bt in
    begin
        print_endline(string_of_bool (search ct 5));
    end ;;

main () ;;
