(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
 Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec reverse lst = 
  let rec rev_acc acc lst = function
    | [] -> acc
    | x :: xs -> rev_acc (x :: acc) xs
  in
  rev_acc [] lst

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat c n = if n <= 0 then [] else c :: (repeat c (n-1))

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
Pri tem ne smete uporabbiti vgrajene funkcije [List.init].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
 acc je prazen seznam, ki je pomozen argument pri repnih rekurzijah. shranjuje trenutno stanje funkcije
[*----------------------------------------------------------------------------*)

 
let range n = 
  let rec range_aux n acc = 
    if n <= 0 then acc else range_aux (n-1) (n :: acc)
  in 
  range_aux n []

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 Pri tem ne smete uporabiti vgrajene funkcije [List.map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map xs

(*----------------------------------------------------------------------------*]
 Časovna zahtevnost operatorja [@] je linearna v prvem argumentu, poskušajte 
 napisati reverse tako, da bo bolj učinkovit in hkrati repno rekurziven.
 Pri tem ne smete uporabiti vgrajene funkcije [List.rev] ali [List.rev_append].
[*----------------------------------------------------------------------------*)

let rec reverse = ()

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map_tlrec f list= 
  let map_tlrec_aux f acc = function
    | [] -> List.rev acc
    | x :: xs -> map_tlrec_aux f (f x :: acc) xs


(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 Pri tem ne smete uporabiti vgrajene funkcije [List.mapi].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let rec mapi f list =
  let mapi_aux list i =
    match list with
    |[] -> []
    |x :: xs -> (f x i) :: 

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije [List.combine].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let rec zip list =
  match (list1, list2) with
  |([], []) -> []
  |([], _) | (_, []) -> failwith "Different lengths of input lists."
  | (x :: xs, y :: ys) -> (x, y) :: (zip xs ys)

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 Pri tem ne smete uporabiti vgrajene funkcije [List.split].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> []
  | (x, y) :: xs -> (x :: unzip xs, y :: unzip xs)

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let unzip_tlrec list = 
  let rec unzip_tlrec_aux acc = function
    | [] -> acc
    | (x, y) :: xs -> unzip_tlrec_aux [x :: acc1, y :: acc2] xs
  in unzip_tlrec_aux list [[], []]
    

(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)

let rec loop condition f x =
  if condition x then
    let x = f x in 
    loop condition f x
  else x

  

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f = function
  | [] | [_] -> failwith "List too short."
  | x :: y :: [] -> f x y
  | x :: y :: tail -> fold_left_no_acc f ((f x y) :: tail)

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let apply_sequence f x n = 
  let rec apply_sequence_aux f x n acc =
    if n = 0 then List. rev acc else apply_sequence_aux f (f x) (n-1) (f x :: acc)
  in apply_sequence_aux f x n [x]

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 Pri tem ne smete uporabiti vgrajene funkcije [List.filter].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f = function
  | [] -> []
  | x :: xs -> if f x then x :: filter xs else filter xs
 
(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true], in [false] sicer.
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first = ()
