(**********************************************************************)
(***************************** Exercice 1 *****************************)
(**********************************************************************)
print_string "------------------- Exercice 1 -------------------\n\n";;

(* 1.1. *)
(* val interval : int -> int -> int list = <fun> *)
let interval a b =
    let rec aux a' b' acc = 
        if a' > b' then 
            acc
        else
            aux (a' + 1) b' (acc @ [a']) 
    in
    aux a b [];;

interval 0 8;;
interval 2 5;;
interval 3 3;;
interval (-5) 2;;
interval 9 3;;

(* 1.2. *)
(* val string_of_list : 'a list -> ('a -> string) -> string = <fun> *)
let string_of_list lst f = 
    let func acc e = String.concat "" [acc; f e]
    in
    List.fold_left func "" lst;;

string_of_list [0; 1; 2] string_of_int;;
string_of_list [] string_of_int;;
string_of_list [true; false; true] string_of_bool;;
string_of_list [2; 0; 101; 0; 0] (fun x -> if x = 0 then "." else "*");;

(* 1.3. *)
(* val compose_iter : ('a -> 'a) -> 'a -> int -> 'a list = <fun> *)
let compose_iter f x n =
    let rec aux i prec acc = 
        if i = n then
            acc
        else
            let curr = f prec in
                aux (i + 1) (curr) (acc @ [curr])
    in
    aux 0 x [x];;    

compose_iter (fun x -> x + 1) 0 5;;
compose_iter (fun x -> -x) 8 6;;
compose_iter (fun u -> u ^ "a") "b" 5;;
compose_iter (fun u -> u) "a" 0;;

(* 1.4. *)
(* val is_prefix_lists : 'a list -> 'a list -> bool = <fun> *)
let rec is_prefix_lists l1 l2 = 
    match (l1, l2) with
    | [], lst   -> true
    | lst, []   -> false
    | e1 :: tl1, e2 :: tl2 when e1 = e2 -> is_prefix_lists tl1 tl2 
    | _         -> false;;

is_prefix_lists [2; 1; 2; 3] [2; 1; 2; 3; 6; 2; 7];;
is_prefix_lists [] [2; 1; 2; 3];;
is_prefix_lists [2; 1] [];;
is_prefix_lists [] [];;
is_prefix_lists ['a'; 'b'; 'b'] ['a'; 'b'; 'b'];;
is_prefix_lists [2; 1; 3] [2; 1; 2; 3; 6; 2; 7];;

(* 1.5. *)
(* val is_factor_lists : 'a list -> 'a list -> bool = <fun> *)
let rec is_factor_lists l1 l2 =
    if is_prefix_lists l1 l2 then
        true
    else
        if l2 = [] then
            false
        else
            is_factor_lists l1 (List.tl l2);;

is_factor_lists [2; 1; 3] [4; 2; 1; 3; 4];;
is_factor_lists [2; 1; 3] [2; 1; 3; 4];;
is_factor_lists [2; 1; 3] [4; 2; 1; 3];;
is_factor_lists ['a'; 'a'] ['a'; 'b'; 'a'];;
is_factor_lists [] ['a'];;
is_factor_lists [] [];;

(* 1.6. *)
(* val is_subword_lists : 'a list -> 'a list -> bool = <fun> *)
let rec is_subword_lists l1 l2 =
    match (l1, l2) with
    | [], lst   -> true
    | lst, []   -> false
    | e1 :: tl1, e2 :: tl2 when e1 = e2 -> is_subword_lists tl1 tl2
    | e1 :: tl1, e2 :: tl2              -> is_subword_lists l1 tl2;;

is_subword_lists [1; 3; 5] [1; 2; 3; 4; 5];;
is_subword_lists [1; 5; 3] [1; 2; 3; 4; 5];;
is_subword_lists ['a'; 'b'] ['a'; 'b'; 'c'];;
is_subword_lists [] ['a'];;
is_subword_lists [] [];;

(* 1.7. *)
(* val is_duplicate_free : 'a list -> bool = <fun> *)
let rec is_duplicate_free lst = 
    match lst with
    | []        -> true
    | e :: tl   -> (not (List.mem e tl)) && is_duplicate_free tl;;

is_duplicate_free [1; 2; 3];;
is_duplicate_free [1; 2; 1];;
is_duplicate_free [1; 2; 1; 1; 2];;
is_duplicate_free [3];;
is_duplicate_free [];;

(**********************************************************************)
(***************************** Exercice 2 *****************************)
(**********************************************************************)
print_string "------------------- Exercice 2 -------------------\n\n";;

type 'a automaton = {
    ribbon : int -> 'a;
    evol : 'a * 'a * 'a -> 'a;
    void : 'a
}

(* 2.1. *)
(* val create : ('a * 'a * 'a -> 'a) -> 'a -> 'a automaton = <fun> *)
let create evol void = 
    { 
        ribbon = (fun i -> void);
        evol = evol;
        void = void
    };;

create (fun (a, b, c) -> a + b + c) 0;;
create (fun (a, b, c) -> b) true;;
create (fun (a, b, c) -> a ^ b ^ c) "";;

(* 2.2. *)
(* val get_value : 'a automaton -> int -> 'a = <fun> *)
let get_value aut i = 
    aut.ribbon i;;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
get_value aut1 0;;
get_value aut1 1024;;

let aut2 = create (fun (a, b, c) -> b) true;;
get_value aut2 (-2048);;

(* 2.3. *)
(* val set_value : 'a automaton -> int -> 'a -> 'a automaton = <fun> *)
let set_value aut i x =
    let new_ribbon j = 
        if i = j then
            x
        else
            get_value aut j
    in 
    { aut with ribbon = new_ribbon };;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut2 = set_value aut1 16 4;;

get_value aut2 15;;
get_value aut2 16;;
get_value aut1 16;;

(**********************************************************************)
(***************************** Exercice 3 *****************************)
(**********************************************************************)
print_string "------------------- Exercice 3 -------------------\n\n";;

type bunch = int * int

(* 3.1. *)
(* val get_bunch_values : 'a automaton -> bunch -> 'a list = <fun> *)
let rec get_bunch_values aut bunch =
    let i, j = bunch in
        List.map (fun x -> get_value aut x) (interval i j)
   
let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 (-1) 2;;

get_bunch_values aut1 (-2, 6);;

(* 3.2. *)
(* val to_string : 'a automaton -> bunch -> ('a -> string) -> string
    = <fun> *)
let rec to_string aut bunch f =
        string_of_list (get_bunch_values aut bunch) f;;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 (-1) 2;;

to_string aut1 (-2, 6) string_of_int;;
to_string aut1 (-2, 6) (fun x -> if x = 0 then "." else "*");;

(* 3.3. *)
(* val has_factor : 'a automaton -> bunch -> 'a list -> bool = <fun> *)
let has_factor aut bunch lst =
    is_factor_lists lst (get_bunch_values aut bunch);;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 (-1) 2;;
let aut1 = set_value aut1 5 9;;

has_factor aut1 (1, 8) [4; 0; 9; 0];;
has_factor aut1 (1, 5) [4; 0; 9; 0];;

(* 3.4. *)
(* val has_subword : 'a automaton -> bunch -> 'a list -> bool = <fun> *)
let has_subword aut bunch lst = 
    is_subword_lists lst (get_bunch_values aut bunch);;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 (-1) 2;;
let aut1 = set_value aut1 5 9;;

has_subword aut1 (1, 8) [4; 9];;
has_subword aut1 (7, 8) [4; 9];;

(**********************************************************************)
(***************************** Exercice 4 *****************************)
(**********************************************************************)
print_string "------------------- Exercice 4 -------------------\n\n";;

(* 4.1. *)
(* val shift : 'a automaton -> int -> 'a automaton = <fun> *)
let shift aut k =
    let new_ribbon x =
        get_value aut (x + k)
    in
    { aut with ribbon = new_ribbon};;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 (-1) 2;;
to_string aut1 (-2, 6) string_of_int;;

let aut2 = shift aut1 3;;
to_string aut2 (-2, 6) string_of_int;;

let aut3 = shift aut1 (-4);;
to_string aut3 (-2, 6) string_of_int;;
to_string aut3 (-2, 12) string_of_int;;

(* 4.2. *)
(* val mirror : 'a automaton -> 'a automaton = <fun> *)
let mirror aut =
    let new_ribbon x = 
        get_value aut (-x)
    in
    { aut with ribbon = new_ribbon };;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 (-1) 2;;
to_string aut1 (-8, 8) string_of_int;;

let aut2 = mirror aut1;;
to_string aut2 (-8, 8) string_of_int;;

(* 4.3. *)
(* val map : 'a automaton -> ('a -> 'a) -> 'a automaton = <fun> *)
let map aut f =
    let new_ribbon x = 
        f (get_value aut x)
    in
    { aut with ribbon = new_ribbon };;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 (-1) 2;;
to_string aut1 (-8, 8) string_of_int;;

let aut2 = map aut1 (fun x -> x + 1);;
to_string aut2 (-8, 8) string_of_int;;

(* 4.4. *)
(* val evolution : 'a automaton -> 'a automaton = <fun> *)
let evolution aut = 
    let new_ribbon x = 
        let x1 = get_value aut (x - 1) and x2 = get_value aut x and x3 = get_value aut (x + 1) in
            aut.evol (x1, x2, x3)
    in
    { aut with ribbon = new_ribbon };;

let aut1 = create (fun (a, b, c) -> a + b + c) 0;;
let aut1 = set_value aut1 3 4;;
let aut1 = set_value aut1 2 1;;
let aut1 = set_value aut1 (-1) 2;;
to_string aut1 (-8, 8) string_of_int;;

let aut2 = evolution aut1;;
to_string aut2 (-8, 8) string_of_int;;

(* 4.5. *)
(* val evolutions : 'a automaton -> int -> 'a automaton list = <fun> *)
let evolutions aut n = 
    compose_iter evolution aut n;;

let aut = create (fun (a, b, c) -> a + b) 0;;
let aut = set_value aut 0 1;;
let lst = evolutions aut 4;;

(* 4.6. *)
(* val evolutions_bunch : 'a automaton -> bunch -> int -> 'a list list
    = <fun> *)
let evolutions_bunch aut b n = 
    let rec aux lst =
        match lst with 
        | []        -> []
        | aut :: tl -> (get_bunch_values aut b) :: (aux tl)
    in
    aux (evolutions aut n);;

let aut = create (fun (a, b, c) -> a + b) 0;;
let aut = set_value aut 0 1;;

evolutions_bunch aut (-1, 5) 4;;

(* 4.7. *)
(* val is_resurgent : 'a automaton -> bunch -> int -> bool *)

let is_resurgent aut b n =
    not (is_duplicate_free (evolutions_bunch aut b n));;


let aut = create (fun (a, b, c) -> a + b) 0;;
let aut = set_value aut 0 1;;

is_resurgent aut (-1, -1) 4;;
is_resurgent aut (-1, 0) 4;;
is_resurgent aut (-1, 1) 4;;

(**********************************************************************)
(***************************** Exercice 5 *****************************)
(**********************************************************************)
print_string "------------------- Exercice 5 -------------------\n\n";;

(* 5.1. *)
(* val sierpinski : int automaton
    = {ribbon = <fun>; evol = <fun>; void = 0} *)

let sierpinski = create (fun (a, b, c) -> (a + b + c) mod 2) 0;;

let aut = set_value sierpinski 0 1;;

print_string (String.concat "\n"
    (List.map
        (fun a -> to_string a (-8, 8) string_of_int)
        (evolutions aut 8)));;

(* 5.2. *)
(* Type wb. *)
(* val chaos : wb automaton
    = {ribbon = <fun>; evol = <fun>; void = White} *)

type wb = Black | White;;

let chaos = 
    let f = fun (a, b, c) ->
        if a = Black && b = Black && c = Black then
            White
        else if a = Black && b = Black && c = White then
            White
        else if a = Black && b = White && c = Black then
            White
        else if a = Black && b = White && c = White then
            Black
        else if a = White && b = Black && c = Black then
            Black
        else if a = White && b = Black && c = White then
            Black
        else if a = White && b = White && c = Black then
            Black
        else 
            White
    in 
    create f White;;

let aut = set_value chaos 0 Black;;

print_string (String.concat "\n"
    (List.map
        (fun a -> to_string a (-8, 8)
            (fun x -> if x = Black then "*" else "."))
        (evolutions aut 8)));;

(**********************************************************************)
(***************************** Exercice 6 *****************************)
(**********************************************************************)
print_string "------------------- Exercice 6 -------------------\n\n";;

let aut = set_value sierpinski 0 1;;

(* 6.1. *)
(* evolutions aut 16;;

print_string (String.concat "\n"
    (List.map
        (fun a -> to_string a (-8, 8) string_of_int)
        (evolutions aut 16)));; *)

(* La phrase précédente met énormément de temps pour charger. *)

(* 6.2. *)
