type couleur = Rouge|Noir;;
type arbreRN = Vide|Noeud of couleur * arbreRN * int * arbreRN;;

#load "graphics.cma";;
open Graphics;;

let pas_h = 20;;
let pas_v = 80;;
let x0 = 20;;
let y0 = 800;;
let cote = 16
let rayon = 15;;

let f c k= Noeud(c, Vide, k, Vide);;

let a0 = Noeud(Noir,
               Noeud(Rouge, f Noir 11, 13, Noeud(Noir, f Rouge 16, 18, Vide)),
               22,
               Noeud(Noir, Vide, 28, f Rouge 31)
              );;


let dessin_noeud arbre x y =
  match arbre with
    |Vide ->  fill_rect (x-cote/2) (y-cote/2) cote cote
    |Noeud(c, g, r, d) -> 
        if c = Rouge then set_color red;
        fill_circle x y rayon;
        set_color white;
        let ch = string_of_int r in
        let k = String.length ch in
          moveto (x - 3*k + 1) (y - 5);
          draw_string ch;
          set_color black;;


let dessin arbre =
  let rec aux x y a =
    match a with
      |Vide -> x, y, x
      |Noeud(c, g, r, d)-> 
          let y_fils = y - pas_v in
          let x1, y1, xr1 = aux x y_fils g in
          let xr = x1 + pas_h in
          let x2, y2, xr2 = aux (xr + pas_h) y_fils d in
            moveto xr1 y_fils;
            lineto xr y;
            lineto xr2 y_fils;
            dessin_noeud g xr1 y_fils;
            dessin_noeud d xr2 y_fils;
            x2, (min y1 y2), xr in
    open_graph " 800x900";
    let a, b, xr = aux x0 y0 arbre in dessin_noeud arbre xr y0;
      let _ = wait_next_event [Button_down] in  close_graph ();;

dessin a0;;
