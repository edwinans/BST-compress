(* partie 2 *)

open Constructor ;;

type c_tree = 
    | Empty
    | C_Node of c_tree ref * int array
    | Node of c_tree * int * c_tree * int * int;;


let rec phi (t : binary_tree) = match t with
    | Empty -> ""
    | Node(l, e, r) -> "(" ^ (phi l) ^ ")" ^ (phi r);;


(* example Q1.7 *)
let test_tree = ref (construct [4;2;3;8;1;9;6;7;5]);;

let rec prefix (t : binary_tree) = match t with 
    | Empty -> []
    | Node(l, e, r) -> e :: prefix l @ prefix r ;;

let prefix_array (t : binary_tree) = Array.of_list (prefix t)

let rec find l s = match l with 
    | [] -> None 
    | (x, t) :: q -> if x = s then Some(t) else find q s ;;

let rec c_size (t : c_tree) = match t with
    | Empty -> 0
    | Node(l, _, r, _, _) -> 1 + c_size l + c_size r
    | C_Node(_, a) -> Array.length a ;;

let rec c_get_size (t : c_tree) = match t with 
    | Empty -> (0, 0)
    | Node(_, _, _, ls, rs) -> (ls, rs)
    | C_Node(tree, _) -> c_get_size !tree ;;

let compress (t : binary_tree) : c_tree = 
    let patterns = ref [] in

    let rec aux (t:binary_tree)  = match t with
    | Empty -> Empty
    | Node(l, e, r) -> 
        match find !patterns (phi t) with
        | Some(tree) -> C_Node(tree, prefix_array t)
        | None ->
            let left = 
            match find !patterns (phi l) with 
            | None -> aux l 
            | Some(tree) -> C_Node(tree, prefix_array l) in 

            let right =
            match find !patterns (phi r) with 
            | None -> aux r 
            | Some(tree) -> C_Node(tree, prefix_array r) in 

        let node = ref (Node(left, e, right, c_size left , c_size right)) in
        (patterns:= (phi t, node) :: !patterns ; !node) in  
    aux t ;;


let rec search (t : c_tree) x = 
    let rec aux t x i arr = match t with
    | Empty -> false 
    | Node(l, e, r, ls, rs) -> 
        let elem = if arr = [||] then e else arr.(i) in
        if x = elem then true else if x<elem then
            if arr=[||] then aux l x i arr else aux l x (i+1) arr else
            if arr=[||] then aux r x i arr else aux r x (i+1+ls) arr      
    | C_Node(tree, a) ->
        let ls = fst(c_get_size !tree) and
        array = if arr = [||] then a else arr in 
        let e = array.(i) in
        if x = e then true else
            if ls=0 then false else
                if x<e then aux !tree x (i+1) array 
                else aux !tree x (i+1+ls) array in 
    aux t x 0 [||] ;;

(* Q2.12 complexite en moyenne : n(log n) *)

