(* parites 1.1 et 1.2 *)

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

let range i j = 
    (* tail recursive *)
    let rec aux j acc = 
        if i==j then (i :: acc) else aux (j-1) (j :: acc) in 
    (aux j [])

let gen_permutation n =
    (* tail recursive *)
    let rec aux (l,p) = 
        if List.length l = 0 then p 
        else aux (extraction_alea l p) in 
    aux (range 1 n,[])

(* Q1.3 : O(n) en nb d'appelle a Random.int et O(n^2) en nb de match *)

let intercale l1 l2 = 
    (* tail recursive *)
    let rec aux l1 l2 n1 n2 acc = match l1, l2 with 
        | [] , [] -> acc
        | l, [] -> l @ acc
        | [], l -> l @ acc 
        | x :: xs, y :: ys -> (Random.self_init ();
            let r = Random.int (n1 + n2) in 
            if r < n1 then aux xs l2 (n1-1) n2 (x :: acc)
            else aux l1 ys n1 (n2-1) (y :: acc)) in 
    List.rev (aux l1 l2 (List.length l1) (List.length l2) [])

let rec gen_permutation2 p q = 
    if p>q then [] else if p = q then [p] else 
    intercale (gen_permutation2 p ((p+q)/2)) (gen_permutation2 ((p+q)/2 + 1) q) 

(* Q1.6 : n(log n) en nb de Random.int et n(log n) en nb de match*)