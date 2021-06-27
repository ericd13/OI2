type graphe = int list array;;

let g0:graphe = [|[]; [2; 3]; [1; 3]; [1; 2]; [5]; [4]|];;

let g1:graphe = [|[1]; [2]; [3; 5]; [1]; [5]; [4]|];;

let g2:graphe = [|[1]; [0]; [4]; [0; 2]; [3]; [4]|];;

let g3:graphe = [|[1; 4]; [2]; [7; 3]; [5; 1]; [6]; [4; 7]; [5]; []|];;

let taille (g:graphe) = Array.length g;;

let voisins (g:graphe) s = g.(s);;
