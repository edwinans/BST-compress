(* partie 2 *)

type binary_tree =
    | Empty
    | Node of binary_tree * int * binary_tree;;


 let rec insert t x = match t with
    | Empty -> Node(Empty, x, Empty)
    | Node(l, e, r) ->
       if x = e then t
       else if x < e then Node(insert l x, e, r)
       else Node(l, e, insert r x)

let construct = List.fold_left insert Empty  


type c_tree = 
    | Empty
    | C_Node of c_tree ref * int array
    | Node of c_tree * int * c_tree ;;


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

        let node = ref (Node(left, e, right)) in
        (patterns:= (phi t, node) :: !patterns ; !node) in  
    aux t ;;

