(* -------------------- Exercice 1 -------------------- *) print_string "-------------------- Exercice 1 --------------------";;
type point = float * float;;

(* ------- Question 1 ------- *) print_string "\n------- Question 1 -------";;
let make_point a b = 
    let p : point = (a, b) in p;;

make_point 1.5 2.;;
make_point (-1.0) 2.5;;

(* ------- Question 2 ------- *) print_string "\n------- Question 2 -------";;
let point_x (p : point) : float = fst p;;
let point_y (p : point) : float = snd p;;

let p = make_point 1. 2.;;
point_x p;;
point_y p;;

(* ------- Question 3 ------- *) print_string "\n------- Question 3 -------";;
let point_domination p1 p2 = 
    (point_x p1) >= (point_x p2) && (point_y p1) >= (point_y p2);;

let p1 = make_point 0. 0. and p2 = make_point 0. 1. and p3 = make_point 1. 1.;;
point_domination p1 p1, point_domination p1 p2, point_domination p1 p3;;
point_domination p2 p1, point_domination p2 p2, point_domination p2 p3;;
point_domination p3 p1, point_domination p3 p2, point_domination p3 p3;;


(* -------------------- Exercice 2 -------------------- *) print_string "\n-------------------- Exercice 2 --------------------";;
type rectangle = point * point;;

(* ------- Question 1 ------- *) print_string "\n------- Question 1 -------";;
let make_rectangle (p1 : point) (p2 : point) =
    assert (point_domination p2 p1);
    let rect : rectangle = (p1, p2) in rect;;

let p1 = make_point 0. 0. and p2 = make_point 1. 2.;;
make_rectangle p1 p2;;

(* ------- Question 2 ------- *) print_string "\n------- Question 2 -------";;
let rectangle_lower_left (rect : rectangle) : point = fst rect;;
let rectangle_upper_right (rect : rectangle) : point = snd rect;;

let r = make_rectangle (make_point 0. 0.) (make_point 1. 2.);;
rectangle_lower_left r;;
rectangle_upper_right r;;

(* ------- Question 3 ------- *) print_string "\n------- Question 3 -------";;
let rectangle_width rect =  
    let p1 = point_x (rectangle_upper_right rect) and p2 = point_x (rectangle_lower_left rect) in
        p1 -. p2;;

let rectangle_height rect = 
    let p1 = point_y (rectangle_upper_right rect) and p2 = point_y (rectangle_lower_left rect) in
        p1 -. p2;;

let r = make_rectangle (make_point 0. 0.) (make_point 1. 2.);;
rectangle_width r;;
rectangle_height r;;

(* ------- Question 4 ------- *) print_string "\n------- Question 4 -------";;
let rectangle_contains_point rect p = 
    let l = rectangle_lower_left rect and r = rectangle_upper_right rect in
        (point_domination p l) && (point_domination r p);;

let r = make_rectangle (make_point 0. 0.) (make_point 1. 1.);;
let p1 = make_point 0. 0.
and p2 = make_point 0.5 0.5
and p3 = make_point 0.5 1.5;;

rectangle_contains_point r p1;;
rectangle_contains_point r p2;;
rectangle_contains_point r p3;;

(* ------- Question 5 ------- *) print_string "\n------- Question 5 -------";;
let rectangle_contained_points rect points =
    List.filter (rectangle_contains_point rect) points;;

let r = make_rectangle (make_point 0. 0.) (make_point 1. 1.);;
let p1 = make_point 0. 0.
and p2 = make_point 0.5 0.5
and p3 = make_point 0.5 1.5;;

rectangle_contained_points r [p1; p2; p3];;

(* -------------------- Exercice 3 -------------------- *) print_string "\n-------------------- Exercice 3 --------------------";;
type quadtree =
    | Leaf of point list * rectangle
    | Node of quadtree * quadtree * quadtree * quadtree * rectangle;;

(* ------- Question 1 ------- *) print_string "\n------- Question 1 -------";;
let rectangle_split_four rect = 
    let xl, yl = rectangle_lower_left rect and xr, yr = rectangle_upper_right rect in
        let r1 = make_rectangle (make_point xl (yr /. 2.)) (make_point (xr /. 2.) yr)
        and r2 = make_rectangle (make_point (xr /. 2.) (yr /. 2.)) (make_point xr yr)
        and r3 = make_rectangle (make_point (xr /. 2.) yl) (make_point xr (yr /. 2.))
        and r4 = make_rectangle (make_point xl yl) (make_point (xr /. 2.) (yr /. 2.))
            in (r1, r2, r3, r4);;

let r = make_rectangle (make_point 0. 0.) (make_point 1. 2.);;
rectangle_split_four r;;

(* ------- Question 2 ------- *) print_string "\n------- Question 2 -------";;
let smallest l =
    let f acc x = if (compare acc x) < 0 then acc else x in
        List.fold_left f (List.hd l) l;;

let greatest l =
    let f acc x = if (compare acc x) > 0 then acc else x in
        List.fold_left f (List.hd l) l;;

let l = [2; 4; 6; 1; 8; 4; 3; 1];;
smallest l;;
greatest l;;

(* ------- Question 3 ------- *) print_string "\n------- Question 3 -------";;
let enclosing_rectangle points = 
    let rect : rectangle = (smallest points, greatest points) in rect;;
    
let p1 = make_point 0. 0. and p2 = make_point 0.5 0.5 and p3 = make_point 0.5 1.5;;
enclosing_rectangle [p1; p2; p3];;

(* ------- Question 4 ------- *) print_string "\n------- Question 4 -------";;
let quadtree_make points n =
    let rec filter_l2 l1 l2 =
        let rec aux e l2 = 
            match l2 with 
            | [] -> []
            | hd :: tl when hd = e -> tl
            | hd :: tl -> hd :: (aux e tl)
        in
        match l1 with
        | []    -> l2
        | hd :: tl -> 
    in
    let rec aux points' n rect = 
        if (List.length points') <= n then
            Leaf (points', rect)
        else
            let r1, r2, r3, r4 = rectangle_split_four rect in
                let p1 = rectangle_contained_points r1 points' in
                    let points_2 = filter_l2 p1 points' [] in
                    let p2 = rectangle_contained_points r2 points_2 in
                        let points_3 = filter_l2 p2 points_2 [] in
                        let p3 = rectangle_contained_points r3 points_3 in
                            let points_4 = filter_l2 p3 points_3 [] in
                            let p4 = rectangle_contained_points r4 points_4 in
                    Node (
                        aux p1 n r1,
                        aux p2 n r2,
                        aux p3 n r3,
                        aux p4 n r4,
                        rect
                    )
    in
    aux points n (enclosing_rectangle points);;

let p1 = make_point 0. 0.
and p2 = make_point 0.5 0.5
and p3 = make_point 0.5 1.5;;

quadtree_make [p1; p2; p3] 1;;

(* ------- Question 5 ------- *) print_string "\n------- Question 5 -------";;
let rec quadtree_count qt = 
    match qt with
    | Leaf (l, _)               -> List.length l
    | Node (q1, q2, q3, q4, _)  -> quadtree_count q1 + quadtree_count q2 + quadtree_count q3 + quadtree_count q4;;

let p1 = make_point 0. 0.
and p2 = make_point 0.5 0.5
and p3 = make_point 0.5 1.5;;         
quadtree_count (quadtree_make [p1; p2; p3] 1);;

(* ------- Question 6 ------- *) print_string "\n------- Question 6 -------";;
let rec quadtree_signature qt =
    match qt with 
    | Leaf (l, _)               -> [List.length l]
    | Node (q1, q2, q3, q4, _)  -> quadtree_signature q1 @ quadtree_signature q2 @ quadtree_signature q3 @ quadtree_signature q4;;

let p1 = make_point 0. 0.
and p2 = make_point 0.5 0.5
and p3 = make_point 0.5 1.5;;  
quadtree_signature (quadtree_make [p1; p2; p3] 1);;

(* -------------------- Exercice 4 -------------------- *) print_string "\n-------------------- Exercice 4 --------------------";;
(* ------- Question 1 ------- *) print_string "\n------- Question 1 -------";;
let rec quadtree_all_points qt =
    match qt with
    | Leaf (l, _)               -> l
    | Node (q1, q2, q3, q4, _)  -> quadtree_all_points q1 @ quadtree_all_points q2 @ quadtree_all_points q3 @ quadtree_all_points q4;;

let p1 = make_point 0. 0. and p2 = make_point 0.5 0.5 and p3 = make_point 0.5 1.5;;
quadtree_all_points (quadtree_make [p1; p2; p3] 1);;

(* ------- Question 2 ------- *) print_string "\n------- Question 2 -------";;
let rectangle_contains_rectangle r1 r2 =
    rectangle_contains_point r1 (rectangle_lower_left r2)
    && rectangle_contains_point r1 (rectangle_upper_right r2);;

let rectangle_disjoint_rectangle r1 r2 =
    let r1_lower_left = rectangle_lower_left r1 in
    let r1_upper_right = rectangle_upper_right r1 in
    let r2_lower_left = rectangle_lower_left r2 in
    let r2_upper_right = rectangle_upper_right r2 in
        point_domination r2_lower_left r1_upper_right || point_domination r1_lower_left r2_upper_right;;

let rectangle_intersection_rectangle r1 r2 =
    let r1_ll = rectangle_lower_left r1 in
    let r1_ur = rectangle_upper_right r1 in
    let r2_ll = rectangle_lower_left r2 in
    let r2_ur = rectangle_upper_right r2 in
    let ll_x_max = greatest [point_x r1_ll; point_x r2_ll] in
    let ll_y_max = greatest [point_y r1_ll; point_y r2_ll] in
    let ur_x_min = smallest [point_x r1_ur; point_x r2_ur] in
    let ur_y_min = smallest [point_y r1_ur; point_y r2_ur] in
    let ll = make_point ll_x_max ll_y_max in
    let ur = make_point ur_x_min ur_y_min in
        make_rectangle ll ur;;

let rec quadtree_rectangle_query rect qt = 
    match qt with
    | Leaf (points, r) when (rectangle_disjoint_rectangle rect r) -> 
        let r' = rectangle_intersection_rectangle rect r in
            rectangle_contained_points r' points

    | Node (q1, q2, q3, q4, r) as node when (rectangle_disjoint_rectangle rect r) -> 
        if (rectangle_contains_rectangle rect r) then
            quadtree_all_points node
        else
            quadtree_rectangle_query rect q1 @ quadtree_rectangle_query rect q2 @ 
            quadtree_rectangle_query rect q3 @ quadtree_rectangle_query rect q4

    | _ -> [];;

(* -------------------- Exercice 5 -------------------- *) print_string "\n-------------------- Exercice 5 --------------------";;
(* let quadtree_insert qt n point =
    let rec aux qt' rect =
        match qt' with
        | Leaf (l, r)               -> 
            if 

        | Node (q1, q2, q3, q4, r)  -> 
             *)

let points = [ (make_point 0. 0.); (make_point 0.1 0.2); (make_point 0.01 0.01); (make_point 2. 0.5); (make_point 0.5 1.5)];;
let r = enclosing_rectangle points;;
let qt = quadtree_make points 2;;
let count = quadtree_count qt;;
let signature = quadtree_signature qt;;
let all_points = quadtree_all_points qt;;
let rec range i j = if i > j then [] else i::(range (i+1) j);;
let cartesian l l' = List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l);;

let points m n =
    let xs = List.map float_of_int (range 0 m) and
        ys = List.map float_of_int (range 0 n) in
        List.map (fun (x, y) -> make_point x y ) (cartesian xs ys);;

let ps = points 4 4;;


let qt = quadtree_make ps 2;; 

(* 
let points_in = let p1 = make_point 0. 0. and p2 = make_point 2. 3. in
    let query = make_rectangle p1 p2 in
        quadtree_rectangle_query query qt;;

let points_in = let p1 = make_point 3. 3. and p1 = make_point 5. 5. in
    let query = make_rectangle p1 p2 in
        quadtree_rectangle_query query qt;;

let points_in = let p1 = make_point 2. (-1.) and p2 = make_point 3. 5. in
    let query = make_rectangle p1 p2 in
        quadtree_rectangle_query query qt;;

let points_in = let p1 = make_point 3.75 3.75 and p2 = make_point 6. 6. in
    let query = make_rectangle p1 p2 in
        quadtree_rectangle_query query qt;;

let points_in = let p1 = make_point 4.75 4.75 and p2 = make_point 6. 6. in
    let query = make_rectangle p1 p2 in
        quadtree_rectangle_query query qt;;
*)


