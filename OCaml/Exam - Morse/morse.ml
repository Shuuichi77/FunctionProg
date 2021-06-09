(* ----------- Exercice 1 ----------- *)
(* ---- Question 1 ---- *)
let char_list_of_string str =
    let rec aux str n acc = 
        if n = 0 then
            acc
        else
            aux str (n - 1) ((String.get str (n - 1)) :: acc)
    in aux str (String.length str) [];;

char_list_of_string "pinguin";;


(* ---- Question 2 ---- *)
let string_of_char_list lst = 
    let rec aux lst acc = 
        if lst = [] then
            acc
        else
            aux (List.tl lst) (String.concat "" [acc; Char.escaped(List.hd lst)])
    in aux lst "";;

string_of_char_list ['p'; 'a'; 'n'; 'g'; 'o'; 'l'; 'i'; 'n'];;


(* ---- Question 3 ---- *)
let join_strings_with_string sep lst = 
    String.concat sep lst;;
    
join_strings_with_string " - " ["cat";"dog";"bird";"fly"];;

(* ---- Question 4 ---- *)
let join_chars_with_string sep lst = 
    let lst' = List.map (fun x -> Char.escaped x) lst in
    join_strings_with_string sep lst';;

join_chars_with_string "*" ['p'; 'a'; 'n'; 'g'; 'o'; 'l'; 'i'; 'n'];;


(* ---- Question 5 ---- *)
let rec contains_only l1 l2 =
    let func x = List.mem x l2
    in List.for_all func l1;;

contains_only ['b';'a';'c'] ['a';'b';'c';'d'];;
contains_only ['b';'i';'c'] ['a';'b';'c';'d'];;

(* ---- Question 6 ---- *)
let rec fzip f l1 l2 =
    match l1, l2 with
    | [], _ -> []
    | _, [] -> []
    | hd1 :: tl1, hd2 :: tl2 -> (f hd1 hd2) :: (fzip f tl1 tl2);;

fzip (+) [1;2] [5;6;7];;
fzip (+) [1;2;3] [5;6;7];;


(* ---- Question 7 ---- *)
let rec zip l1 l2 =
    let func a b = a, b 
    in fzip func l1 l2;;

zip [1;2] [5;6;7];;


(* ---- Question 8 ---- *)
let unzip lists =
    let func (next_a, next_b) (acc_a, acc_b) = (next_a :: acc_a, next_b :: acc_b)
    in List.fold_right func lists ([], []);;

unzip (zip [1;2;3] [5;6;7]);;

(* ---- Question 9 ---- *)
type morse = Dash | Dot | Blank | NotAMorseSymbol

type morse_letter = morse list

(* val blank_translation : (char * morse list) list *)
let blank_translation : (char * morse_letter) list =
  [' ', [Blank]];;

(* val num_translation : (char * morse list) list *)
let num_translation : (char * morse_letter) list =
    [ '1', [Dot; Dash; Dash; Dash; Dash];
    '2', [Dot; Dot; Dash; Dash; Dash];
    '3', [Dot; Dot; Dot; Dash; Dash];
    '4', [Dot; Dot; Dot; Dot; Dash];
    '5', [Dot; Dot; Dot; Dot; Dot];
    '6', [Dash; Dot; Dot; Dot; Dot];
    '7', [Dash; Dash; Dot; Dot; Dot];
    '8', [Dash; Dash; Dash; Dot; Dot];
    '9', [Dash; Dash; Dash; Dash; Dot];
    '0', [Dash; Dash; Dash; Dash; Dash] 
    ]

(* val char_translation : (char * morse list) list *)
let char_translation : (char * morse_letter) list =
  [
  'a', [Dot; Dash];
  'b', [Dash; Dot; Dot; Dot];
  'c', [Dash; Dot; Dash; Dot];
  'd', [Dash; Dot; Dot];
  'e', [Dot];
  'f', [Dot; Dot; Dash; Dot];
  'g', [Dash; Dash; Dot];
  'h', [Dot; Dot; Dot; Dot];
  'i', [Dot; Dot];
  'j', [Dot; Dash; Dash; Dash];
  'k', [Dash; Dot; Dash];
  'l', [Dot; Dash; Dot; Dot];
  'm', [Dash; Dash];
  'n', [Dash; Dot];
  'o', [Dash; Dash; Dash];
  'p', [Dot; Dash; Dash; Dot];
  'q', [Dash; Dash; Dot; Dash];
  'r', [Dot; Dash; Dot];
  's', [Dot; Dot; Dot];
  't', [Dash];
  'u', [Dot; Dot; Dash];
  'v', [Dot; Dot; Dot; Dash];
  'w', [Dot; Dash; Dash];
  'x', [Dash; Dot; Dot; Dash];
  'y', [Dash; Dot; Dash; Dash];
  'z', [Dash; Dash; Dot; Dot]
  ]

(* morse code for chars ’a’, ’b’, .., ’z’, ’0’, ’1’, ..., ’9’ *)
let morse_translation = blank_translation @ num_translation @ char_translation;;

(* ---- Question 9 ---- *)
let check_msg str = 
    let alphabet, _ = unzip morse_translation in
    contains_only (char_list_of_string str) alphabet;;

check_msg "happy otter day";;
check_msg "Happy Otter Day!";;


(* ---- Question 10 ---- *)
let morse_letter_of_char char =
    let _, morse = List.find (fun (x, _) -> x = char) morse_translation in morse;;

morse_letter_of_char 'a';;


(* ---- Question 11 ---- *)
let char_of_morse_letter morse =
    let char, _ = List.find (fun (_, x) -> x = morse) morse_translation in char;;

char_of_morse_letter (morse_letter_of_char 'a');;


(* ---- Question 12 ---- *)
let morse_code message =
    if check_msg message then
        let func char = (morse_letter_of_char char)
        in List.map func (char_list_of_string message)
    else
        [];;

let msg = "happy otter day";;
let morse_code_example = morse_code msg;;

(* ---- Question 13 ---- *)
type morse_rendering = RDash | RDot | SymbSpace | LetterSpace | WordSpace| NotAMorseRenderingSymbol

let rec render_letter_code morse =
    match morse with
    | [] -> []
    | [Blank] -> [WordSpace]
    | hd :: tl when hd = Dot    -> if tl = [] then [RDot] else RDot :: (SymbSpace :: render_letter_code tl)
    | hd :: tl when hd = Dash   -> if tl = [] then [RDash] else RDash :: (SymbSpace :: render_letter_code tl)
    | hd :: tl when hd = NotAMorseSymbol   -> if tl = [] then [NotAMorseRenderingSymbol] else NotAMorseRenderingSymbol :: (SymbSpace :: render_letter_code tl)
    | _ -> [NotAMorseRenderingSymbol];;

render_letter_code (morse_letter_of_char 'b');;

(* ---- Question 14 ---- *)
let render_morse_code morse_code = 
    let rec aux lst acc = 
        if lst = [] then
            acc
        else
            if List.tl lst = [] then
                (acc @ (render_letter_code (List.hd lst)))
            else if (List.hd (List.tl lst)) <> [Blank] then
                aux (List.tl lst) (acc @ (render_letter_code (List.hd lst)) @ [LetterSpace])
            else
                aux (List.tl lst) (acc @ (render_letter_code (List.hd lst)))
    in aux morse_code [];;

let render_morse_translation_msg = render_morse_code morse_code_example;;

(* ---- Question 15 ---- *)
let split_list x lst = 
    let func next (temp, acc) = 
        if next = x then
            if temp <> [] then
                ([], [temp] @ acc)
            else 
                ([], acc)
        else
            (next :: temp, acc)
    in let a, b = List.fold_right func lst ([], [])
    in [a] @ b;;

split_list 0 [1; 0; 2; 3; 0; 0; 4];;


(* ---- Question 16 ---- *)
let join_list_with x lst = 
    let func next acc = (x :: next) @ acc
    in if lst = [] then [] else List.tl (List.fold_right func lst []);;

join_list_with 0 [[1]; [2;3]; [4]];;

(* ---- Question 17 ---- *)
let rendering_to_morse rendering =
  match rendering with
  | RDot -> Dot
  | RDash -> Dash
  | WordSpace -> Blank
  | _ -> NotAMorseSymbol


(* ---- Question 18 ---- *)
let render_morse_decode morse_render =
  morse_render
  |> List.filter (fun it -> it <> SymbSpace)
  |> List.map rendering_to_morse
  |> split_list Blank
  |> List.map (split_list NotAMorseSymbol)
  |> join_list_with [Blank]

let morse_decode morse = 
  morse
  |> List.map char_of_morse_letter
  |> string_of_char_list