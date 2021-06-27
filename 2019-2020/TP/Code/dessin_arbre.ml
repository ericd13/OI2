#load "graphics.cma";;
open Graphics;;

type arbreAVL = Vide|Noeud of  arbreAVL * int * arbreAVL * int;;

let x0 = 30;;      (* Position extrème à gauche *)
let y0 = 850;;     (* Hauteur de la racine *)
let rayon = 15;;   (* Rayon des représentation des noeuds *)
let cote = 16;;    (* Coté du carré pour les feuilles *)
let dX = 20;;      (* Ecart horizontal entre deux noeuds *)
let dY = -80;;     (* Ecart vertical (vers le bas) entre deux niveaux *)

let rec squelette arbre x0 y0 =
  set_color black;
  match arbre with
    |Vide -> x0, x0
    |Noeud(g, r, d, h) -> 
        let xrg, xbg = squelette g x0 (y0 + dY) in
        let xr = xbg + dX in 
        let xrd, xbd = squelette d (xr + dX) (y0 + dY) in
          moveto xrg (y0 + dY);
          lineto xr y0;
          lineto xrd (y0 + dY);
          xr, xbd;;

let dessinNoeud arbre x y =
  (* Dessin de la racine d'un arbre à la position (x,y)*)
  match arbre with
    |Vide -> set_color black;
        fill_rect (x-cote/2) (y-cote/2) cote cote
    |Noeud(_, n, _ , _) -> 
        set_color black;
        fill_circle x y rayon;
        set_color white;
        if n < 10 then moveto (x - 2) (y - 5)
        else moveto (x - 6) (y - 5);
        draw_string (string_of_int n);;


let rec noeuds arbre x0 y0 =
  match arbre with
    |Vide -> dessinNoeud Vide x0 y0; x0
    |Noeud(g, n, d, h) -> 
        let xg = noeuds g x0 (y0 + dY) in
        let xr = xg + dX in
        let xd = noeuds d (xr + dX) (y0 + dY) in
          dessinNoeud arbre xr y0 ;
          xd;;

let dessin arbre x0 y0 = 
  let rec auxDessin arbre x y = 
    match arbre with
      |Vide -> (x + dX, x)
      |Noeud(g, n, d, h) -> 
          let (xg, xrg) = auxDessin g x (y + dY) in                     
          let (xd, xrd) = auxDessin d (xg+dX) (y + dY) in
            set_color black;
            moveto xrg (y + dY);
            lineto xg y;
            lineto xrd (y + dY);
            dessinNoeud g xrg (y + dY);
            dessinNoeud d xrd (y + dY);
            (xd, xg) in
  let max, xr = auxDessin arbre x0 y0 in 
    dessinNoeud arbre xr y0;
    wait_next_event [Button_down];;


let feuille n =  Noeud(Vide, n, Vide, 0);;

let rec chercher x arbre = 
  match arbre with
    |Vide -> false
    |Noeud(g, n, d, h) when (n = x) -> true
    |Noeud(g, n, d, h) when (x < n) -> chercher x g
    |Noeud(g, n, d, h) -> chercher x d;;

let ht arbre = 
  match arbre with
    |Vide -> -1
    |Noeud(g, n, d, h) -> h;;

let cons g n0 d = 
  let hg = ht g in
  let hd = ht d in
  let h = (max hg hd) + 1 in 
    Noeud(g, n0, d, h);;

let noeud g n0 d =
  let hg = ht g in
  let hd = ht d in
    match (hg - hd) with
      |2 -> begin match g with
          |Noeud(gg, rg, gd, _) when ht gg >= ht gd 
            -> cons gg rg (cons gd n0 d)
          |Noeud(gg, rg, Noeud(gdg, rgd, gdd, _), _)
            -> cons (cons gg rg gdg) rgd (cons gdd n0 d)
          | _ -> failwith "Ceci ne devrait pas arriver" end
      |(-2) -> begin match d with
          |Noeud(dg, rd, dd, _) when ht dd > ht dg
            -> cons (cons g n0 dg) rd dd
          |Noeud(Noeud(dgg, rdg, dgd, _), rd, dd, _) 
            -> cons (cons g n0 dgg) rdg (cons dgd rd dd)
          | _ -> failwith "Ceci ne devrait pas arriver" end
      | _ -> cons g n0 d;;

let rec noeud g n0 d =
  let hg = ht g in
  let hd = ht d in
    if (hg - hd) > 1
    then begin match g with
      |Noeud(gg, rg, gd, _) when ht gg >= ht gd 
        -> noeud gg rg (noeud gd n0 d)
      |Noeud(gg, rg, Noeud(gdg, rgd, gdd, _), _)
        -> noeud (noeud gg rg gdg) rgd (noeud gdd n0 d)
      | _ -> failwith "Ceci ne devrait pas arriver" end
    else if (hg - hd) < -1 
    then begin match d with
      |Noeud(dg, rd, dd, _) when ht dd > ht dg
        -> cons (cons g n0 dg) rd dd
      |Noeud(Noeud(dgg, rdg, dgd, _), rd, dd, _) 
        -> cons (cons g n0 dgg) rdg (cons dgd rd dd)
      | _ -> failwith "Ceci ne devrait pas arriver" end
    else cons g n0 d;;

let rec maxArbre arbre = 
  match arbre with
    |Vide -> raise(Failure "Arbre vide")
    |Noeud(_, r, Vide, _) -> r
    |Noeud(_, _, d, _) -> maxArbre d;;

let rec suppressionMax arbre = 
  match arbre with
    |Vide -> failwith "Arbre vide"
    |Noeud(g, r, Vide, h) -> g
    |Noeud(g, r, d, h) -> noeud g r (suppressionMax d);;

let suppressionRacine arbre = 
  match arbre with
    |Vide -> Vide
    |Noeud(Vide, r, d, h) -> d
    |Noeud(g, r, d, h) ->  let p = maxArbre g in
          noeud (suppressionMax g) p d;;

let rec suppression n arbre = 
  match arbre with
    |Vide -> Vide
    |Noeud(g, r, d, h) when n = r 
      -> suppressionRacine arbre
    |Noeud(g, r, d, h) when n < r 
      -> noeud (suppression n g) r d
    |Noeud(g, r, d, h) -> noeud g r (suppression n d);;


let rec insertion n arbre = 
  match arbre with
    |Vide -> feuille n
    |Noeud(g, p, d, h) when n = p-> arbre
    |Noeud(g, p, d, h) when n < p
      -> noeud (insertion n g)  p  d
    |Noeud(g, p, d, h) -> noeud g p (insertion n d);;

let rec decoupage n arbre = 
  match arbre with
    |Vide -> (Vide,Vide)
    |Noeud(g, r, d, h) when n = r -> (g, d)
    |Noeud(g, r, d, h) when n < r
      -> let (gg, gd) = decoupage n g 
        in (gg, noeud gd r d)
    |Noeud(g, r, d, h) -> let (dg, dd) = decoupage n d 
        in (noeud g r dg, dd);;

let insertionRacine n arbre =
  let (g, d)= decoupage n arbre
  in noeud g n d;;



let a0 = Noeud(Noeud(feuille 11,
                     13,
                     Noeud(feuille 16, 18, Vide, 1),
                     2
                    ),
               22,
               Noeud(Vide, 28, feuille 31, 1),
               3
              );;

let evolution liste =
  let n = List.hd liste in
  let rec aux liste arbre =
    match liste with
      |[] -> let a1 = suppression n arbre in 
          let _ = dessin a1 x0 y0 in
            clear_graph();
      |n::reste -> let a1 = insertionRacine n arbre in
          let _ = dessin a1 x0 y0 in
            clear_graph();
            aux reste a1 in
    aux liste Vide;
    close_graph();;



open_graph " 800x900";;
evolution  [7; 11; 2; 6; 9; 4; 12; 5; 14; 8; 1];;
