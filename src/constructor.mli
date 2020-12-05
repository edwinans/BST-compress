
type binary_tree =
    | Empty
    | Node of binary_tree * int * binary_tree

val insert : binary_tree -> int -> binary_tree

val search : binary_tree -> int -> bool

val construct : int list -> binary_tree