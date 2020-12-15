open Constructor

type c_tree = 
    | Empty
    | C_Node of c_tree * int array            (*pointeur vers un arbre - tableau prefixe*)
    | Node of c_tree * int * c_tree * int     (*fg - etiquette - fd - taille(fg)*)

val compress : binary_tree -> c_tree

val search : c_tree -> int -> bool