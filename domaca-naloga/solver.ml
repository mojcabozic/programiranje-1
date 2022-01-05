let box_index (vrstica, stolpec) = stolpec/3 + (vrstica/3)*3

let rec int_in_list i = function
    | [] -> false
    | x :: xs -> if Some x = Some i then true else int_in_list i xs 
    

let is_valid i (vrstica, stolpec) (grid: Model.problem) =
    not (int_in_list i (Model.get_box grid (box_index (vrstica, stolpec))) 
        || int_in_list i (Array.to_list (Model.get_column grid stolpec)) 
        || int_in_list i (Array.to_list (Model.get_row grid vrstica)))
    
let rec next_empty grid (vrstica, stolpec) =
    if vrstica = 8 && stolpec = 8 then (-1, -1)
    else
        if stolpec != 8 then 
            if grid.(vrstica).(stolpec + 1) = None then (vrstica, stolpec + 1)
            else next_empty grid (vrstica, stolpec + 1)
        else
            if grid.(vrstica + 1).(0) = None then (vrstica + 1, 0)
            else next_empty grid (vrstica + 1, 0)
      

let solve_problem (grid: Model.problem) =
    let rec solve_grid_aux i (vrstica, stolpec) grid =
        if i = 10 then None
        else
            if (vrstica, stolpec) = (-1, -1) then Some (Model.map_grid Option.get grid)
            else
                if grid.(vrstica).(stolpec) = None then
                    if is_valid (Some i) (vrstica, stolpec) grid then
                        (grid.(vrstica).(stolpec) <- (Some i);
                        if Option.is_none (solve_grid_aux 1 (next_empty grid (vrstica, stolpec)) grid) then
                            (grid.(vrstica).(stolpec) <- None;
                            solve_grid_aux (i + 1) (vrstica, stolpec) grid)
                        else Some (Model.map_grid Option.get grid))
                    else solve_grid_aux (i + 1) (vrstica, stolpec) grid
                else 
                    solve_grid_aux i (next_empty grid (vrstica, stolpec)) grid
    in solve_grid_aux 1 (0, 0) grid
