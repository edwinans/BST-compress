open Constructor;;

type c_tree = 
    | Empty
    | C_Node of c_tree ref * int array
    | Node of c_tree * int * c_tree * int * int;;

val compress : binary_tree -> c_tree

val search : c_tree -> int -> bool