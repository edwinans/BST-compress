
let remove_nth l i = 
    (* tail recursive *)
    let rec aux l i acc = match l with
        | [] -> failwith "list too short"
        | x :: xs -> if i = 0 then (List.rev acc) @ xs else aux xs (i-1) (x :: acc) in 
    (aux l i [])

let extraction_alea l p =
    let _ = Random.self_init () in
    let r = Random.int (List.length l) in
    let e = List.nth l r in
    (remove_nth l r, e :: p)

let range n = 
    (* tail recursive *)
    let rec aux n acc = 
        if n==0 then acc else aux (n-1) (n :: acc) in 
    (aux n [])

let gen_permutation n =
    (* tail recursive *)
    let rec aux (l,p) = 
        if List.length l = 0 then p 
        else aux (extraction_alea l p) in 
    aux (range n,[])

(* Q1.3 : O(n) en nb d'appelle a Random.int et O(n^2) en de match *)


