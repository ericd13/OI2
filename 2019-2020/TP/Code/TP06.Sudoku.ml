type case = Vide | Val of int ;;

let init () =
  Array.make_matrix 9 9 Vide;;

let copie grille =
  let g = init() in
    for i = 0 to 8 do
      for j = 0 to 8 do
        g.(i).(j) <- grille.(i).(j) done; done;
    g;;

let verifTaille grille =
  (Array.length grille = 9) && (Array.length grille.(0) = 9);;

let case grille ligne colonne =
  grille.(ligne).(colonne);;

let possible grille ligne colonne valeur =
  let rep = ref true in
    for i = 0 to 8 do
      if i <> ligne && (case grille i colonne) = Val(valeur)
      then rep := false;
      if i <> colonne && (case grille ligne i) = Val(valeur)
      then rep := false done;
    let a = ligne/3 and b = colonne/3 in
      for i = 0 to 2 do
        let u = 3*a + i in
          for j = 0 to 2 do 
            let v  = 3*b +j in
              if (u <> ligne || v <> colonne) && (case grille u v) = Val(valeur)
              then rep := false done; done;
      !rep;;


let imprime grille =
  let h = " --- --- ---\n" in
    print_newline ();
    print_string h;
    for i = 0 to 8 do    
      print_string "|";
      for j = 0 to 8 do
        (match grille.(i).(j) with
          |Vide -> print_string "x"
          |Val(n) -> print_int n);
        if (j+1) mod 3 = 0
        then print_string "|" done;
      print_newline ();
      if (i+1) mod 3 = 0
      then print_string h done;;

let exemple =
  let g = init() in
  let un = [|1; 4; 7; 2; 5; 8; 3; 6; 9|] in
    for i = 0 to 8 do
      for j = 0 to 8 do
        g.(i).(j) <- Val ((un.(i)+j-1) mod 9 + 1) done; done;
    g;;


exception Solution;;

let suivant (i,j) =
  if j = 8
  then (i + 1,0)
  else (i,j + 1);;

let resoudre grille = 
  let g = copie grille in
  let rec aux g (lg,col) = 
    if lg = 9
    then raise Solution
    else if case g lg col <> Vide
    then aux g (suivant (lg,col))
    else for n = 1 to 9 do
        if possible g lg col n
        then (g.(lg).(col) <- Val n;
              aux g (suivant (lg,col));
              g.(lg).(col) <- Vide) done in
    try aux g (0,0);
      failwith "pas de solution"
    with Solution -> imprime g;;


(* let s = init();;
   s.(0).(3) <- Val(5);;
   possible s 0 6 5;;


   verif s 1 1 5;;

   imprime s;;

   let ss = copie s;;

   ss.(2).(2) <- Val 3;;
   imprime s;;
   imprime ss;;
   imprime exemple;;*)

let test = [|
  [| Val 5  ; Vide   ; Vide   ; Val 2  ; Vide   ; Val 4  ; Vide   ; Vide   ; Vide  |];
  [| Vide   ; Vide   ; Vide   ; Vide   ; Val 6  ; Vide   ; Vide   ; Val 8  ; Vide  |];
  [| Val 7  ; Vide   ; Vide   ; Val 5  ; Vide   ; Val 8  ; Val 4  ; Val 3  ; Vide  |];
  [| Val 1  ; Val 5  ; Vide   ; Val 6  ; Vide   ; Vide   ; Val 2  ; Vide   ; Vide  |];
  [| Vide   ; Val 6  ; Val 3  ; Vide   ; Val 2  ; Vide   ; Val 8  ; Val 1  ; Vide  |];
  [| Vide   ; Vide   ; Val 8  ; Vide   ; Vide   ; Val 3  ; Vide   ; Val 6  ; Val 9 |];
  [| Vide   ; Val 9  ; Val 5  ; Val 3  ; Vide   ; Val 2  ; Vide   ; Vide   ; Val 1 |];
  [| Vide   ; Val 1  ; Vide   ; Vide   ; Val 7  ; Vide   ; Vide   ; Vide   ; Vide  |];
  [| Vide   ; Vide   ; Vide   ; Val 9  ; Vide   ; Val 5  ; Vide   ; Vide   ; Val 8 |]|];;

let test2 = [|
  [| Vide   ; Val 4  ; Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Val  3 ; Vide  |];
  [| Vide   ; Vide   ; Val 3  ; Vide   ; Val  8 ; Val  2 ; Vide   ; Val  1 ; Vide  |];
  [| Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val  8 ; Vide   ; Val 2 |];
  [| Val 9  ; Val 1  ; Vide   ; Val  6 ; Val  2 ; Vide   ; Vide   ; Vide   ; Vide  |];
  [| Val 8  ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val 4 |];
  [| Vide   ; Vide   ; Vide   ; Vide   ; Val  1 ; Val  5 ; Vide   ; Val  7 ; Val 9 |];
  [| Val 3  ; Vide   ; Val  9 ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Vide  |];
  [| Vide   ; Val 8  ; Vide   ; Val  9 ; Val  4 ; Vide   ; Val  6 ; Vide   ; Vide  |];
  [| Vide   ; Val 7  ; Vide   ; Vide   ; Val  6 ; Vide   ; Vide   ; Val  9 ; Vide  |]|];;

let difficile1 = [|
  [| Val 7  ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val  4 ; Vide   ; Vide  |];
  [| Vide   ; Val  2 ; Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Val  8 ; Vide  |];
  [| Vide   ; Vide   ; Val  3 ; Vide   ; Vide   ; Val  8 ; Vide   ; Vide   ; Val 9 |];
  [| Vide   ; Vide   ; Vide   ; Val  5 ; Vide   ; Vide   ; Val  3 ; Vide   ; Vide  |];
  [| Vide   ; Val  6 ; Vide   ; Vide   ; Val  2 ; Vide   ; Vide   ; Val  4 ; Vide  |];
  [| Vide   ; Vide   ; Val  1 ; Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Val 6 |];
  [| Vide   ; Vide   ; Vide   ; Val  3 ; Vide   ; Vide   ; Val  9 ; Vide   ; Vide  |];
  [| Vide   ; Val  3 ; Vide   ; Vide   ; Val  4 ; Vide   ; Vide   ; Val  6 ; Vide  |];
  [| Vide   ; Vide   ; Val  9 ; Vide   ; Vide   ; Val  1 ; Vide   ; Vide   ; Val 5 |]|];;

let difficile2 = [|
  [| Vide   ; Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Val  8 ; Vide   ; Vide   |];
  [| Vide   ; Vide   ; Vide   ; Vide   ; Val  4 ; Vide   ; Vide   ; Val  3 ; Vide   |];
  [| Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val  9 ; Vide   ; Vide   ; Val  1 |];
  [| Val  6 ; Vide   ; Vide   ; Val  5 ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   |];
  [| Vide   ; Val  1 ; Vide   ; Vide   ; Val  3 ; Vide   ; Vide   ; Val  4 ; Vide   |];
  [| Vide   ; Vide   ; Val  5 ; Vide   ; Vide   ; Val  1 ; Vide   ; Vide   ; Val  7 |];
  [| Val  5 ; Vide   ; Vide   ; Val  2 ; Vide   ; Vide   ; Val  6 ; Vide   ; Vide   |];
  [| Vide   ; Val 3  ; Vide   ; Vide   ; Val  8 ; Vide   ; Vide   ; Val  9 ; Vide   |];
  [| Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val  2 |]|];;




