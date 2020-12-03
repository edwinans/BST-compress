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