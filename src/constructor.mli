
type binary_tree =
    | Empty
    | Node of binary_tree * int * binary_tree;;

val construct : int list -> binary_tree