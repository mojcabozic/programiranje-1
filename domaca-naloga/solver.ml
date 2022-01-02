type sudoku_grid = int option Array.t Array.t

let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let chunkify_grid (grid : sudoku_grid) =
    Array.map (fun a -> chunkify 3 (Array.to_list a)) grid

let rec get_row (grid : sudoku_grid) (row_ind : int) = 
    grid.(row_ind)

let get_column (grid : sudoku_grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind)) 

let get_box (grid : sudoku_grid) (box_ind : int) = 
    let grid_chunks = chunkify_grid grid in 
        match box_ind with
        | 0 | 1 | 2 -> List.flatten [List.nth grid_chunks.(0) box_ind; List.nth grid_chunks.(1) box_ind; List.nth grid_chunks.(2) box_ind]
        | 3 | 4 | 5 -> List.flatten [List.nth grid_chunks.(3) (box_ind - 3); List.nth grid_chunks.(4) (box_ind - 3); List.nth grid_chunks.(5) (box_ind - 3)]
        | 6 | 7 | 8 -> List.flatten [List.nth grid_chunks.(6) (box_ind - 6); List.nth grid_chunks.(7) (box_ind - 6); List.nth grid_chunks.(8) (box_ind - 6)]
    
(* skopirano *)


let box_index (vrstica, stolpec) = stolpec/3 + (vrstica/3)*3

let rec int_in_list i = function
    | [] -> false
    | x :: xs -> if Some x = Some i then true else int_in_list i xs 
    

let is_valid i (vrstica, stolpec) (grid: sudoku_grid) =
    not (int_in_list i (get_box grid (box_index (vrstica, stolpec))) || int_in_list i (Array.to_list (get_column grid stolpec)) || int_in_list i (Array.to_list (get_row grid vrstica)))
    
let rec next_empty grid (vrstica, stolpec) =
    if vrstica = 8 && stolpec = 8 then (-1, -1)
    else
        if stolpec != 8 then 
            if grid.(vrstica).(stolpec + 1) = None then (vrstica, stolpec + 1)
            else next_empty grid (vrstica, stolpec + 1)
        else
            if grid.(vrstica + 1).(0) = None then (vrstica + 1, 0)
            else next_empty grid (vrstica + 1, 0)
      

let solve_grid grid =
    let rec solve_grid_aux i (vrstica, stolpec) grid =
        if i = 10 then [|[|None|]|]
        else
            if (vrstica, stolpec) = (-1, -1) then grid
            else
                if grid.(vrstica).(stolpec) = None then
                    if is_valid (Some i) (vrstica, stolpec) grid then
                        (grid.(vrstica).(stolpec) <- (Some i);
                        if solve_grid_aux 1 (next_empty grid (vrstica, stolpec)) grid = [|[|None|]|] then
                            (grid.(vrstica).(stolpec) <- None;
                            solve_grid_aux (i + 1) (vrstica, stolpec) grid)
                        else grid)
                    else solve_grid_aux (i + 1) (vrstica, stolpec) grid
                else 
                    solve_grid_aux i (next_empty grid (vrstica, stolpec)) grid
    in solve_grid_aux 1 (0, 0) grid
