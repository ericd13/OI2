let disjoints (min1, max1) (min2, max2) = 
  max1 < min2 || max2 < min2;;

let fusion (min1, max1) (min2, max2) = 
  (min min1 min2), (max max1 max2);;

type arbre = Vide
           |Feuille of int * int
           |Noeud of arbre * int * arbre;;

let a0 = Noeud(Noeud(Feuille(11, 14), 
                     14, 
                     Feuille(18, 23)
                    ), 
               23,
               Feuille(31, 37)
              );;               

let rec suite arbre = 
  match arbre with
    |Vide -> []
    |Feuille(a, b) -> [(a, b)]
    |Noeud(g, _, d) -> (suite g) @ (suite d);;

let verifie a =
  let rec aux a = 
    match a with
      |Vide -> true, 0, 0
      |Feuille(min, max) -> true, min, max
      |Noeud(g, k, d) -> let rep1, min1, max1 = aux g in
          let rep2, min2, max2 = aux d in
          let rep = rep1 && rep2 && (max1 = k) && (k < min2) in
            rep, min1, max2 in
  let rep, _, _ = aux a in rep;;

let rec appartient k a =
  match a with
    |Vide -> false
    |Feuille(min, max) -> (min <= k) && (k <= max)
    |Noeud(g, n, d) when n < k ->  appartient k d
    |Noeud(g, n, d) -> appartient k g;;


let rec ajouter (min, max) a =
  match a with
    |Vide -> Feuille( min, max)
    |Feuille(min1, max1) when max1 < min -> 
        Noeud(a, max1, Feuille(min, max))
    |Feuille(min1, max1) -> Noeud( Feuille(min, max), max, a)
    |Noeud(g, n, d) when n < min -> Noeud(g, n, ajouter (min, max) d)
    |Noeud(g, n, d) -> Noeud(ajouter (min, max) g, n, d);;

suite (ajouter (25, 29) (ajouter (8,10) a0));;
    
