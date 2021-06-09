(* ------------------- Exercice 1 -------------------*)
type tree =
    | Leaf of int
    | Node of char * tree list;;

let example_tree =
    Node ('a',
    [Node ('b',[Leaf 1; Leaf 2; Leaf 3]);
    Node ('c', 
        [Node ('d', [Leaf 4; Leaf 5]); 
        Node ('e', [Leaf 6])]);
    Leaf 7;
    Node ('f', 
        [Node ('g', [Leaf 8]); 
        Leaf 9])]);;

(* ------ Question 1 ------ *)
let tree_count_nodes t =
    let rec aux l =
    match l with
        | [] -> 0
        | (Leaf(_) :: siblings)             -> (aux siblings)
        | (Node(_, childrens) :: siblings)  -> 1 + (aux childrens) + (aux siblings)
    in
    aux [t];;

tree_count_nodes example_tree;;

let tree_count_leaves t = 
    let rec go l = 
    match l with
        | []                              -> 0
        | (Leaf(_) :: siblings)           -> 1 + go siblings
        | (Node(_, children) :: siblings) -> go children + go siblings
    in 
    go [t];;

tree_count_leaves example_tree;;

(* ------ Question 2 ------ *)
let tree_list_leaves t =
    let rec aux l acc = 
        match l with
        | [] -> acc
        | (Leaf(x) :: siblings)             -> aux siblings (x :: acc) (* acc @ [x] pour avoir l'ordre de l'énoncé, mais compléxité plus grande *)
        | (Node(_, children) :: siblings)   -> let acc' = aux children acc in aux siblings acc'
    in
    aux [t] [];;

tree_list_leaves example_tree;;