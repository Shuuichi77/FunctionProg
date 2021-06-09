(* --------------- Exercice 1 --------------- *) print_string "--------------- Exercice 1 ---------------\n\n";;
#load "graphics.cma";;

open Graphics;;

(* Dimensions of the graphical window. *)
let w = 512 and h = 512

(* Type for the representation of functional images. *)
type picture = int * int -> Graphics.color

(* render f : draw the image `f` on the graphical window. *)
let render (f : picture) =
    open_graph (Printf.sprintf " %dx%d" w h);
    auto_synchronize false;
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            set_color (f (x, y));
            plot x y
        done;
        set_color background
    done;
    synchronize ();
    (wait_next_event [Button_down; Key_pressed]) |> ignore; 
    close_graph ()

(* ------- Exemple ------- *)
let black_on_black : picture = 
    fun point -> black;;
            
let half_plane color : picture = 
    fun (x, y) -> 
        if x < w / 2 then 
            color 
        else
            background;;

(* render(half_plane blue);; *)

(* ------- Question 1 ------- *)
let diagonal color : picture = 
    fun (x, y) ->
        if x = y then
            color
        else
            background;;

(* render(diagonal black);; *)

(* ------- Question 2 ------- *)
let square length color : picture = 
    fun (x, y) ->
        (* Left & Right side *)
        if (x = 0 || x = length) && (y >= 0 && y <= length) then
            color
        (* Bottom & Top side *)
        else if (y = 0 || y = length) && (x >= 0 && x <= length) then
            color
        else
            background;;
    
(* render(square 100 black);; *)

(* ------- Question 3 ------- *)
let rectangle length height color : picture = 
    fun (x, y) ->
        (* Left & Right side *)
        if (x = 0 || x = length) && (y >= 0 && y <= height) then
            color
        (* Bottom & Topside *)
        else if (y = 0 || y = height) && (x >= 0 && x <= length) then
            color
        else
            background;;

(* render(rectangle 100 200 black);; *)

(* ------- Question 4 ------- *)
let disk radius color : picture = 
    fun (x, y) ->
        if (x * x + y * y) <= (radius * radius) then
            color
        else
            background;;

(* render(disk 100 black);; *)

(* ------- Question 5 ------- *)
let circle radius color : picture = 
    fun (x, y) ->
        let point = (x * x + y * y) and perimeter = (radius * radius) in
            (* perimeter +/6 radius sinon le cercle est trop précis pour les valeurs de x et y qui sont des entiers*)
            if (point >= (perimeter - radius)) && (point <= (perimeter + radius)) then
                color
            else
                background;;

(* render(circle 100 black);; *)

(* ------- Question 6 ------- *)
let move im (dx, dy) : picture =
    fun (x, y) -> im(x - dx, y - dy);;

(* render(disk 100 blue);;
render(move (disk 100 blue) (0, 100));; *)

(* ------- Question 7 ------- *)
(* render(move (rectangle 128 64 green) (w / 2, h / 2 - 64));; *)

(* ------- Question 8 ------- *)
let vertical_symetry im : picture =
    fun (x, y) -> im(w - x, y)
        
let circle_1 = circle 200 blue;;

(* render(circle_1);;
render(vertical_symetry circle_1);; *)

(* ------- Question 9 ------- *)
let horizontal_symetry im : picture =
    fun (x, y) -> im(x, h - y);;

(* render(circle_1);;
render(horizontal_symetry circle_1);; *)

(* ------- Question 10 ------- *)
(* render (diagonal green);;
render (vertical_symetry (diagonal green));; *)


(* --------------- Exercice 2 --------------- *) print_string "\n--------------- Exercice 2 ---------------\n\n";;
(* ------- Question 1 ------- *)
let v_lines n : picture =
    fun (x, y) ->
        if (x mod n) = 0 then
            black
        else
            background;;

(* render(v_lines 50);; *)

(* ------- Question 2 ------- *)
let v_stripes n : picture =
    fun (x, y) ->
        if (x mod (n * 2)) < n then
            black
        else
            background;;

(* render(v_stripes 50);; *)

(* ------- Question 3 ------- *)
let chessboard color n : picture =
    fun (x, y) ->
        let x_size = x mod (n * 2) and y_size = y mod (n * 2) in
            if (x_size <= n) && (y_size <= n) then
                color
            else if (x_size >= n) && (y_size >= n) then
                color
            else
                background;;

(* render(chessboard black (h / 8));;
render(chessboard blue (h / 4));; *)

(* ------- Question 4 ------- *)
let concentric color n : picture = 
    fun (x, y) ->
        let rec aux dist n' is_color = 
            (* Sa dist > n' * n', on change la couleur du disque, et on regarde si dist sera < au disque suivant *)
            if dist > (n' * n') then
                aux dist (n' + n) (not is_color)                     
            else if is_color then 
                color
            else
                background
        in
        (* On fait la distance de (x, y) par rapport au centre de la fenêtre et non pas (0, 0) pour centrer les cercles *)
        let x' = (w / 2) - x and y' = (h / 2) - y in
            let dist = (x' * x' + y' * y') in
                aux dist n true;;

(* render(concentric blue 50);; *)

(* --------------- Exercice 3 --------------- *) print_string "\n--------------- Exercice 3 ---------------\n\n";;
(* ------- Question 1 ------- *)
let compose_two im1 im2 : picture =
    fun (x, y) ->
        let c1 = im1(x, y) and c2 = im2(x, y) in
            if c2 = background then
                c1
            else
                c2;;

let disque_1 = move (disk (w / 3) red) (w / 3, h / 2);;
let disque_2 = move (disk (w / 3) blue) (w / 2, h / 2);;

(* render(disque_1);;
render(disque_2);;
render(compose_two disque_1 disque_2);; *)

(* ------- Question 2 ------- *)
let mickey_leftear = move (disk (h / 8) black) (w / 4, h * 3 / 4);;
let mickey_rightear = move (disk (h / 8) black) (w * 3 / 4, h * 3 / 4);;
let mickey_head = move (disk (h / 4) black) (w / 2, h / 2);;

let mickey = compose_two (compose_two mickey_leftear mickey_rightear) mickey_head;;
(* render(mickey);; *)

(* ------- Question 3 ------- *)
let rec compose list : picture =
    match list with
    | []                    -> fun white -> background
    | [e]                   -> e
    | e1 :: e2 :: reste     -> compose_two (compose_two e1 e2) (compose reste);;

(* ------- Question 4 ------- *)
let mickey_subhead = move (disk (h / 4 - h / 50) blue) (w / 2, h / 2);;
let mickey_nose = move (disk (h / 20) black) (w / 2, h / 2);;
let mickey_lefteye = move (disk (h / 40) black) (w / 2 + w / 14, h / 2 + h / 14);;
let mickey_righteye = move (disk (h / 40) black) (w / 2 - w / 14, h / 2 + h / 14);;

let mickey_face = compose [mickey; mickey_subhead; mickey_nose; mickey_lefteye; mickey_righteye];;
(* render (mickey_face);; *)

(* --------------- Exercice 4 --------------- *) print_string "\n--------------- Exercice 4 ---------------\n\n";;
let from_polar (rho, theta) =
    (int_of_float (rho *. cos theta), int_of_float (rho *. sin theta));;

let to_polar (x, y) =
    let distance_to_origin (x, y) = sqrt (float_of_int (x * x + y * y)) in
    (distance_to_origin (x, y), atan2 (float_of_int y) (float_of_int x));;

(* ------- Question 1 ------- *)
let rotate im angle : picture = 
    fun (x, y) ->
        let rho, theta = to_polar(x - (w / 2), y - (h / 2)) in
            let x', y' = from_polar(rho, theta -. angle) in
                im(x' + (w / 2), y' + (h / 2));;

(* ------- Question 2 ------- *)
(* render(mickey);; *)

(* 90° *)
(* render(rotate mickey (3. *. Float.pi /. 2.));;  *)

(* 180° *)
(* render(rotate mickey (Float.pi));;  *)

(* 270° *)
(* render(rotate mickey (Float.pi /. 2.));;  *)

(* 0° *)
(* render(rotate mickey (0.));;  *)

(* ------- Question 3 ------- *)
let sun color =
    let disk = move (disk (w / 6) color) (w / 2, h / 2) and ray_size = 3
    
    in    
 
    let line : picture = fun (x, y) -> 
        if (y >= (w / 2 - ray_size)) && (y <= (w / 2 + ray_size)) then 
            color 
        else 
            background 
    
    in 
    
    let rec lines n total acc = 
        if n = 0. then
            acc 
        else 
            lines (n -. 1.) total ((rotate line (n *. Float.pi /. total)) :: acc) 

    and nb_line = 25. 
        
    in compose (lines nb_line nb_line [disk]);;

(* render (sun yellow);; *)

(* ------- Question 4 ------- *)
let compose_xor_two im1 im2 : picture =
    fun (x, y) ->
        let c1 = im1(x, y) and c2 = im2(x, y) in
            if c1 <> background && c2 = background then
                c1
            else if c1 = background && c2 <> background then
                c2
            else
                background;;

(* ------- Question 5 ------- *)
let double_concentric size : picture =
    let im1 = move (concentric black size) (-w / 4, 0) and im2 = move (concentric black size) (w / 4, 0) in
        compose_xor_two im1 im2;;

(* render(double_concentric 50);; *)

(* ------- Question 6 ------- *)
let rec compose_xor list : picture = 
    match list with
    | []                -> fun white -> background
    | [e]               -> e
    | e1 :: e2 :: reste -> compose_xor_two (compose_xor_two e1 e2) (compose_xor reste);;

(* render(compose_xor [concentric black 50; move (concentric black 50) (-w / 4, 0); move (concentric black 50) (-w / 2, 0); move (concentric black 50) (w / 2, 0); move (concentric black 50) (w / 4, 0)]);; *)

(* ------- Question 7 ------- *)
let rosace : picture = 
    let im1 = (double_concentric 50) in
        let im2 = rotate (im1) (Float.pi /. 4.) and im3 = rotate (im1) (3. *. Float.pi /. 4.) and im4 = rotate (im1) (Float.pi /. 2.) in
            compose_xor [im1; im2; im3; im4];;

(* render(rosace);; *)

let rosace_bis : picture = 
    let im1 = (double_concentric 50) in
        let im2 = rotate (im1) (Float.pi /. 3.) and im3 = rotate (im1) (2. *. Float.pi /. 3.) in
            compose_xor [im1; im2; im3];;

(* render(rosace_bis);; *)