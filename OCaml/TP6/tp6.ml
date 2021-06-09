(* ------------------- Exercice 2 : Systeme de Fichiers ------------------- *)
(* file system item name alias *)
type fs_item_name = string

(* file system folder *)
(* a folder is simply decribed by its name *)
type fs_folder = Folder of fs_item_name

(* file system file type *)
type fs_file_type = 
    | PDF   (* portable document format *)
    | DOC   (* microsoft document *)
    | PNG   (* portable network graphics *)
    | JPG   (* joint photographic experts group *)
    | AVI   (* audio video interleave *)
    | MKV   (* matroska video *)

(* file system file size alis *)
type fs_file_size = int

(* file system file *)
(* a file is described by its name, its type and its size *)
type fs_file = File of fs_item_name * fs_file_type * fs_file_size

(* file system item *)
(* a file system item is either a folder (containing items) or a file *)
type fs_item = 
    | FolderItem of fs_folder * fs_item list
    | FileItem of fs_file

(* --------------------------------------------------------------------------------------------------------------- *)

let my_fs =
    FolderItem (Folder "root",
    [FolderItem (Folder "Documents",
    [FileItem (File ("doc 1", DOC, 32)); FileItem (File ("doc 2", DOC, 64));
    FileItem (File ("doc 3", PDF, 1024));
    FileItem (File ("doc 4", PDF, 2048));
    FolderItem (Folder "2015",
    [FileItem (File ("sujet tp note", PDF, 512));
    FileItem (File ("notes tp", DOC, 64))]);
    FolderItem (Folder "2016",
    [FileItem (File ("sujet tp note", PDF, 512));
    FileItem (File ("notes tp", DOC, 64))])]);
    FileItem (File ("Documents", PDF, 512));
    FileItem (File ("config", DOC, 28));
    FolderItem (Folder "Downloads",
    [FileItem (File ("doc 1", DOC, 32)); FileItem (File ("doc 2", DOC, 64));
    FileItem (File ("doc 1", PDF, 1024));
    FileItem (File ("doc 2", PDF, 2048))]);
    FolderItem (Folder "Movies",
    [FolderItem (Folder "Rocky 1",
    [FileItem (File ("Rocky 1", MKV, 4294967296));
    FileItem (File ("Rocky 1 - subtitle fr", DOC, 4096));
    FileItem (File ("Rocky 1 - subtitle en", DOC, 4096))]);
    FolderItem (Folder "Jaws 2",
    [FileItem (File ("Jaws 2", AVI, 16777216));
    FileItem (File ("Jaws 2 - subtitle fr", DOC, 4096))]);
    FolderItem (Folder "Alien 3",
    [FileItem (File ("Alien 3", MKV, 4294967296))]);
    FileItem (File ("Seven", AVI, 1024));
    FileItem (File ("seen movies", DOC, 64))]);
    FolderItem (Folder "Pictures",
    [FileItem (File ("Martine fait du chameau 1", PNG, 2048));
    FileItem (File ("Martine fait du chameau 2", JPG, 4096));
    FolderItem (Folder "Photos 2015",
    [FileItem (File ("description 2015", DOC, 256));
    FileItem (File ("Martine au zoo 1", JPG, 2048));
    FileItem (File ("Martine au zoo 2", JPG, 2048));
    FileItem (File ("Martine au zoo 3", PNG, 2048))]);
    FolderItem (Folder "Photos 2016",
    [FileItem (File ("description 2016", DOC, 512));
    FileItem (File ("Martine mange une pomme 1", JPG, 2048));
    FileItem (File ("Martine mange une pomme 2", JPG, 2048));
    FileItem (File ("Martine mange une pomme 3", PNG, 2048));
    FileItem (File ("Martine mange une pomme 4", PNG, 2048))])])]);;

let my_fs2 =
    FolderItem (Folder "root",
    [FolderItem (Folder "Documents",
    [FileItem (File ("doc␣1", DOC, 32));
    FileItem (File ("doc␣2", DOC, 64));
    FileItem (File ("doc␣3", PDF, 1024));
    FileItem (File ("doc␣4", PDF, 2048))]);
    FileItem (File ("Documents", DOC, 28))]);;

let my_fs3 =
    FolderItem (Folder "root",
    [FolderItem (Folder "Documents",
    [FolderItem (Folder "Documents",
    [FileItem (File ("doc␣1", DOC, 32));
    FileItem (File ("doc␣2", DOC, 64))]);
    FileItem (File ("doc␣1", DOC, 32));
    FileItem (File ("doc␣2", DOC, 64))]);
    FileItem (File ("Downloads", DOC, 28))]);;

let my_fs4 =
    FolderItem (Folder "root",
    [FolderItem (Folder "Documents",
    [FileItem (File ("doc␣1", DOC, 32));
    FileItem (File ("doc␣2", DOC, 64));
    FileItem (File ("doc␣1", PDF, 1024));
    FileItem (File ("doc␣2", PDF, 2048))]);
    FolderItem (Folder "Documents", [])]);;

let my_fs5 =
    FolderItem (Folder "root",
    [FolderItem (Folder "Documents",
    [FileItem (File ("doc␣1", DOC, 32));
    FileItem (File ("doc␣1", DOC, 64));
    FileItem (File ("doc␣3", PDF, 1024));
    FileItem (File ("doc␣4", PDF, 2048))]);
    FileItem (File ("Documents", DOC, 28))]);;

let my_fs6 =
    FolderItem (Folder "root",
    [FolderItem (Folder "Documents",
    [FileItem (File ("doc␣1", DOC, 32));
    FileItem (File ("doc␣1", DOC, 64))]);
    FileItem (File ("AutresDocuments", DOC, 28))]);;

let test1 = File ("file 1", DOC, 32);;
let test2 = File ("file 2", PNG, 512);;
let test3 = File ("file 3", AVI, 4294967296);;

(* --------------------------------------------------------------------------------------------------------------- *)
(* ------------------- Exercice 3 : Fonctions sur les listes de fichiers ------------------- *)
(* ------ Question 1 ------ *) print_string "\n ------------------------------ Question 1 ------------------------------- \n\n"
let files items = 
    let rec aux item acc = 
        match item with
        | []                                        -> acc
        | (FileItem(file) :: siblings)              -> aux siblings (file :: acc)
        | (FolderItem(_, childrens) :: siblings)    -> let acc' = aux childrens acc in aux siblings acc'
    in 
    aux [items] [];;

files my_fs;;

(* ------ Question 2 ------ *) print_string "\n ------------------------------ Question 2 ------------------------------- \n\n"
let folders items = 
    let rec aux item acc =
        match item with
        | []                                            -> acc
        | (FileItem(_) :: siblings)                     -> aux siblings acc
        | (FolderItem(folder, childrens) :: siblings)   -> let acc' = aux childrens (folder :: acc) in aux siblings acc'
    in
    aux [items] [];;

folders my_fs;;

(* ------ Question 3 ------ *) print_string "\n ------------------------------ Question 3 ------------------------------- \n\n"
let is_image file =
    match file with
        | File (_, JPG, _)  -> true
        | File (_, PNG, _)  -> true
        | _                 -> false;;

let is_movie file =
    match file with
        | File (_, AVI, _)  -> true
        | File (_, MKV, _)  -> true
        | _                 -> false;;

let is_document file =
    match file with
        | File (_, PDF, _)  -> true
        | File (_, DOC, _)  -> true
        | _                 -> false;;

is_image test1, is_image test2, is_image test3;;
is_movie test1, is_movie test2, is_movie test3;;
is_document test1, is_document test2, is_document test3;;

(* ------ Question 4 ------ *) print_string "\n ------------------------------ Question 4 ------------------------------- \n\n"
let images items = List.filter is_image (files items);;
let movies items = List.filter is_movie (files items);;
let documents items = List.filter is_document (files items);;

images my_fs;;
movies my_fs;;
documents my_fs;;

(* ------ Question 5 ------ *) print_string "\n ------------------------------ Question 5 ------------------------------- \n\n"
(* let rec rec_search_list items name =
    match items with
    | [] -> []
    | File (file_name, _, _) as file :: tl ->
        if (String.compare file_name name) = 0 then
            file :: (rec_search_list tl name)
        else
            rec_search_list tl name;; *)

let rec rec_search_list items name =
    match items with
    | [] -> []
    | File (file_name, _, _) as file :: tl when file_name = name -> file :: (rec_search_list tl name)
    | hd :: tl -> rec_search_list tl name;;

rec_search_list (files my_fs) "notes td";;
rec_search_list (files my_fs) "notes tp";;

(* ------ Question 6 ------ *) print_string "\n ------------------------------ Question 6 ------------------------------- \n\n"
let tail_rec_search_list items name = 
    let rec aux list name acc =
        if list = [] then
            acc
        else
            let hd = (List.hd list) in
            match hd with
            | File (file_name, _, _) when file_name = name  -> aux (List.tl list) name (hd :: acc)
            | File (file_name, _, _)                        -> aux (List.tl list) name acc
    in
    aux items name [];;

tail_rec_search_list (files my_fs) "notes td";;
tail_rec_search_list (files my_fs) "notes tp";;

(* ------ Question 7 ------ *) print_string "\n ------------------------------ Question 7 ------------------------------- \n\n"
let not_rec_search_list items name = 
    let f acc file = 
        match file with
        | File (file_name, _, _) when file_name = name  -> file :: acc
        | File (file_name, _, _)                        -> acc
    in
    List.fold_left f [] items;;

not_rec_search_list (files my_fs) "notes td";;
not_rec_search_list (files my_fs) "notes tp";;

(* ------ Question 8 ------ *) print_string "\n ------------------------------ Question 8 ------------------------------- \n\n"
let search_documents items name = 
    let rec aux l acc =
        match l with 
        | [] -> acc
        | (FileItem(File(file_name, _, _) as file) :: siblings) when file_name = name   -> aux siblings (file :: acc)
        | (FileItem(File(_, _, _)) :: siblings)                                 -> aux siblings acc
        | (FolderItem(_, childrens) :: siblings) -> let acc' = aux childrens acc in aux siblings acc'
    in 
    aux [items] [];;

search_documents my_fs "notes td";;
search_documents my_fs "notes tp";;
search_documents my_fs "Rocky 1";;

(* ------ Question 9 ------ *) print_string "\n ------------------------------ Question 9 ------------------------------- \n\n"
let search_documents_fun search_fun items name = search_fun (files items) name;;

search_documents_fun rec_search_list my_fs "notes td";;
search_documents_fun tail_rec_search_list my_fs "notes tp";;
search_documents_fun not_rec_search_list my_fs "Rocky 1";;

(* ------ Question 10 ------ *) print_string "\n ------------------------------ Question 10 ------------------------------- \n\n"
let size_images items =
    let f acc file = 
        match file with
        | File (_, _, file_size) -> acc + file_size
    in
    List.fold_left f 0 (images items);;

size_images my_fs;;

(* ------ Question 11 ------ *) print_string "\n ------------------------------ Question 11 ------------------------------- \n\n"
let rec fs_filter f items =
    let rec filter l acc = 
        match l with
        | [] -> acc
        | (FileItem(File(name, _, _)) as file :: siblings) -> 
            if f file then
                filter siblings (name :: acc)
            else
                filter siblings acc
        | (FolderItem(Folder(name), childrens) as folder :: siblings) ->
            if f folder then
                let acc' = filter childrens (name :: acc) in filter siblings acc'
            else
                let acc' = filter childrens acc in filter siblings acc'
    in
    filter [items] [];;

(* ------ Question 12 ------ *) print_string "\n ------------------------------ Question 12 ------------------------------- \n\n -------- Question 12.1 -------- \n\n"
(* -- Quest. 12.1 -- *)
let item_names_with_large6_name items = 
    let f item = 
        match item with
        | FileItem(File(name, _, _))    -> if ((String.length name) >= 6) then true else false
        | FolderItem(Folder(name), _)   -> if ((String.length name) >= 6) then true else false
    in 
    fs_filter f items;;

item_names_with_large6_name my_fs;;

(* -- Quest. 12.2 -- *) print_string "\n -------- Question 12.2 -------- \n\n"
let digit_in_names items =
    (* On regarde chaque lettre d'un string pour voir s'il est égal à un chiffre *)
    let rec string_contains_digit s n length = 
        if n = length then
            false
        else 
            let ascii_letter = Char.code (String.get s n)
            in
            if ascii_letter >= 48 && ascii_letter <= 57 then
                true
            else
                string_contains_digit s (n + 1) length
    in   
    let f item =
        match item with
        | FileItem(File(name, _, _))    -> string_contains_digit name 0 (String.length name)
        | FolderItem(Folder(name), _)   -> string_contains_digit name 0 (String.length name)
    in
    fs_filter f items;;

digit_in_names my_fs;;

(* ------ Question 13 ------ *) print_string "\n ------------------------------ Question 13 ------------------------------- \n\n"
let full_paths items = 
    let rec aux item curr_path paths =
        match item with
        | [] -> paths
        | (FileItem(File(name, _, _)) :: siblings) -> 
            let file_path = String.concat "/" [curr_path; name] in
                aux siblings curr_path (file_path :: paths)

        | (FolderItem(Folder(name), childrens) :: siblings) ->
            let child_path = String.concat "/" [curr_path; name] in
                let acc = aux childrens child_path (child_path :: paths) in
                    aux siblings curr_path acc
    in
    aux [items] "" [];;

full_paths my_fs;;

(* ------ Question 14 ------ *) print_string "\n ------------------------------ Question 14 ------------------------------- \n\n"
let name_with_ext item =
    let f_type_to_string f_type = 
        match f_type with 
        | PDF   -> "pdf"
        | DOC   -> "doc"
        | PNG   -> "png"
        | JPG   -> "jpg"
        | AVI   -> "avi"
        | MKV   -> "mkv"
        (* | _     -> "unknown type" *) (* Raise error ? *)
    in
    match item with
    | FileItem(File(name, f_type, _))   -> String.concat "." [name; (f_type_to_string f_type)]
    | FolderItem(Folder(name), _)       -> name;;

name_with_ext (FileItem (test1));;
name_with_ext my_fs;;

(* ------ Question 15 ------ *) print_string "\n ------------------------------ Question 15 ------------------------------- \n\n"
let no_two_identical_names names =
    let rec aux l =
        if l = [] then
            true
        else if List.mem (List.hd l) (List.tl l) then
            false
        else
            aux (List.tl l)
    in
    aux names;;

no_two_identical_names (item_names_with_large6_name my_fs);;

(* ------ Question 16 ------ *) print_string "\n ------------------------------ Question 16 ------------------------------- \n\n"
let check items =
    let rec aux item files folders = 
        match item with
        | []                                                -> (no_two_identical_names files) && (no_two_identical_names folders)
        | (FileItem(_) as file :: siblings)                 -> aux siblings ((name_with_ext file) :: files) folders
        | (FolderItem(_, childrens) as folder :: siblings)  -> (aux childrens [] []) && (aux siblings files ((name_with_ext folder) :: folders))
    in
    aux [items] [] [];;

check my_fs, check my_fs2, check my_fs3, check my_fs4, check my_fs5, check my_fs6;;

(* ------ Question 17 ------ *) print_string "\n ------------------------------ Question 17 ------------------------------- \n\n"
let du items =
    (* Fonction qui récupère la 2ème valeur d'un couple, qui correspond ici à la taille d'un dossier/fichier *)
    let get_size (_, size) = size
    in
    let rec aux item folder_name folder_size acc =
        match item with
        (* Liste vide : On a visité tout le répertoire courant.
                        Donc on peut ajouter le tuple (folder_name, folder_size) à "acc" et on renvoit le tout *)
        | [] -> (folder_name, folder_size) :: acc
        
        (* FileItem : on ajoute le nom du file à "acc", et on relance "aux" sur le reste de la liste (= "siblings") *)
        | (FileItem(File(_, _, size)) as file :: siblings)  -> aux siblings folder_name (folder_size + size) ((name_with_ext file, size) :: acc)
        
        (* FolderItem : - D'abord, on lance "aux" sur ce folder (= "childrens") 
                        - Ensuite, on doit récupérer la taille de ce folder lorsqu'il aura terminé d'être visité : 
                        pour cela, on récupère le size du 1er couple de "acc" (= "child_size"), couple qui correspond au folder qu'on vient juste de visiter à la ligne précédente 
                        - Enfin, on relance la fonction "aux" sur "siblings", tout en incrémentant "folder_size" de "child_size" qu'on vient de récupérer *)
        | (FolderItem(_, childrens) as folder :: siblings)  -> 
            let acc' = aux childrens (name_with_ext folder) 0 acc in
                let acc'', child_size = acc', get_size (List.hd acc') in
                    aux siblings folder_name (folder_size + child_size) acc''
    in
    (* Vu qu'on lance la fonction en créant une liste d'"items", il va ajouter 2 fois le répertoire de la racine.
       On fait donc un "List.tl" pour omettre le 1er élément de la liste *)
    List.tl (aux [items] "" 0 []);;

du my_fs;;