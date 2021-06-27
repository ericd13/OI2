type noeud = {lettre : char;  poids : int; 
              fg : int;      fd : int};;

type foret= {mutable nb_arbres : int; 
             mutable nb_noeuds : int; 
             table : noeud array};;

let noeud_vide = {lettre='\000'; poids=0; fg=(-1); fd=(-1)};;

let f_ex =
  let table_ex = Array.make 20 noeud_vide in
    table_ex.(0) <- {lettre='g'; poids=2; fg=(-1); fd=6};
    table_ex.(1) <- {lettre='a'; poids=7; fg=4; fd=3};
    table_ex.(2) <- {lettre='b'; poids=4; fg=(-1); fd=(-1)};
    table_ex.(3) <- {lettre='e'; poids=3; fg=5; fd=(-1)};
    table_ex.(4) <- {lettre='f'; poids=5; fg=(-1); fd=(-1)};
    table_ex.(5) <- {lettre='c'; poids=4; fg=(-1); fd=(-1)};
    table_ex.(6) <- {lettre='d'; poids=5; fg=(-1); fd=(-1)};
    {nb_arbres = 3; nb_noeuds = 7; table = table_ex};;

let indice_du_min foret k =
  let mini = ref foret.table.(0).poids in
  let ind_mini = ref 0 in
    for i = 1 to k-1 do
      if foret.table.(i).poids < !mini
      then (ind_mini := i;
            mini := foret.table.(i).poids) done;
    !ind_mini;;

let echange i j foret = 
  let noeud = foret.table.(i) in
    foret.table.(i) <- foret.table.(j);
    foret.table.(j) <- noeud;;

let deux_plus_petits foret = 
  let k = foret.nb_arbres in
    if k >= 2
    then begin let i = indice_du_min foret k     
      in echange i (k-1) foret;
        let j = indice_du_min foret (k-1) 
        in echange j (k-2) foret end;;

let assemblage f = 
  let n = f.nb_noeuds in
  let k = f.nb_arbres in
    if k >= 2
    then begin
      deux_plus_petits f;
      f.table.(n) <- f.table.(k-2);
      f.table.(k-2) <- 
        {lettre = '\000'; fg = (k-1); fd = n;
         poids = f.table.(n).poids + f.table.(k-1).poids};
      f.nb_noeuds <- (n+1);
      f.nb_arbres <- (k-1) 
    end;;

let eval f =
  let n = Array.length f.table in
  let haut = Array.make n 0 in
  let rec ht k h =
    haut.(k) <- h;
    let g = f.table.(k).fg in
      if g >= 0 then ht g (h+1);
      let d = f.table.(k).fd in
        if d >= 0 then ht d (h+1) in
    for i = 0 to (f.nb_arbres - 1) do ht i 0 done;
    let ev = ref 0 in
      for i = 0 to (f.nb_noeuds - 1) 
      do if f.table.(i).lettre != '\000'
        then ev := !ev + haut.(i)*f.table.(i).poids done;
      !ev;;

let huff = 
  let t_huff = Array.make 20 noeud_vide in
    t_huff.(0) <- {lettre='a'; poids=7; fg=(-1); fd=(-1)};
    t_huff.(1) <- {lettre='b'; poids=6; fg=(-1); fd=(-1)};
    t_huff.(2) <- {lettre='c'; poids=8; fg=(-1); fd=(-1)};
    t_huff.(3) <- {lettre='d'; poids=3; fg=(-1); fd=(-1)};
    t_huff.(4) <- {lettre='e'; poids=9; fg=(-1); fd=(-1)};
    t_huff.(5) <- {lettre='f'; poids=2; fg=(-1); fd=(-1)};
    {nb_arbres = 6; nb_noeuds = 6; table = t_huff};;

for i = 1 to 5 do assemblage huff done;;
huff;;
eval huff;;
