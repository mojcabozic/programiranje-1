(* Pomožni tip, ki predstavlja mrežo *)
type 'a grid = 'a Array.t Array.t

(* Model za vhodne probleme *)
type problem = int option grid

(* Model za izhodne rešitve *)
type solution = int grid

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let int_opt_to_str arg = 
    if Option.is_none arg then " " else Int.to_string (Option.get arg)
    
let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

let chunkify_grid grid =
    Array.map (fun a -> chunkify 3 (Array.to_list a)) grid

(* Funkcije za dostopanje do elementov mreže *)
let get_row (grid : problem) (row_ind : int) = 
    grid.(row_ind)

let get_column (grid : problem) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind)) 

let get_box (grid : problem) (box_ind : int) = 
    let grid_chunks = chunkify_grid grid in 
        match box_ind with
        | 0 | 1 | 2 -> List.flatten [List.nth grid_chunks.(0) box_ind; List.nth grid_chunks.(1) box_ind; List.nth grid_chunks.(2) box_ind]
        | 3 | 4 | 5 -> List.flatten [List.nth grid_chunks.(3) (box_ind - 3); List.nth grid_chunks.(4) (box_ind - 3); List.nth grid_chunks.(5) (box_ind - 3)]
        | 6 | 7 | 8 -> List.flatten [List.nth grid_chunks.(6) (box_ind - 6); List.nth grid_chunks.(7) (box_ind - 6); List.nth grid_chunks.(8) (box_ind - 6)]
        | _ -> failwith "impossible"
    
(* Funkcije za ustvarjanje novih mrež *)
let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
    let rec aux acc f grid (x, y) =
        if (x, y) = (8, 8) then 
            (acc.(x).(y) <- f grid.(x).(y);
            acc)
        else
            if y != 8 then 
                (acc.(x).(y) <- f grid.(x).(y);
                aux acc f grid (x, y + 1))
            else
                (acc.(x).(y) <- f grid.(x).(y);
                aux acc f grid (x + 1, 0))
in aux (Array.make_matrix 9 9 0) f grid (0, 0)

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

let print_problem problem : unit = print_grid int_opt_to_str problem

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  grid_of_string cell_of_char str         

let print_solution (solution: solution) = print_grid Int.to_string solution