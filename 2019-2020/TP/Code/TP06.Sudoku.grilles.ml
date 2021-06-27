(* ************************************************************************ *)
(* Grilles										                    *)
(* ************************************************************************ *)
let test1 = [|
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

let test3 = [|
   [| Val 7  ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val  4 ; Vide   ; Vide  |];
   [| Vide   ; Val  2 ; Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Val  8 ; Vide  |];
   [| Vide   ; Vide   ; Val  3 ; Vide   ; Vide   ; Val  8 ; Vide   ; Vide   ; Val 9 |];
   [| Vide   ; Vide   ; Vide   ; Val  5 ; Vide   ; Vide   ; Val  3 ; Vide   ; Vide  |];
   [| Vide   ; Val  6 ; Vide   ; Vide   ; Val  2 ; Vide   ; Vide   ; Val  4 ; Vide  |];
   [| Vide   ; Vide   ; Val  1 ; Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Val 6 |];
   [| Vide   ; Vide   ; Vide   ; Val  3 ; Vide   ; Vide   ; Val  9 ; Vide   ; Vide  |];
   [| Vide   ; Val  3 ; Vide   ; Vide   ; Val  4 ; Vide   ; Vide   ; Val  6 ; Vide  |];
   [| Vide   ; Vide   ; Val  9 ; Vide   ; Vide   ; Val  1 ; Vide   ; Vide   ; Val 5 |]|];;

let test4 = [|
   [| Vide   ; Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Val  8 ; Vide   ; Vide   |];
   [| Vide   ; Vide   ; Vide   ; Vide   ; Val  4 ; Vide   ; Vide   ; Val  3 ; Vide   |];
   [| Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val  9 ; Vide   ; Vide   ; Val  1 |];
   [| Val  6 ; Vide   ; Vide   ; Val  5 ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   |];
   [| Vide   ; Val  1 ; Vide   ; Vide   ; Val  3 ; Vide   ; Vide   ; Val  4 ; Vide   |];
   [| Vide   ; Vide   ; Val  5 ; Vide   ; Vide   ; Val  1 ; Vide   ; Vide   ; Val  7 |];
   [| Val  5 ; Vide   ; Vide   ; Val  2 ; Vide   ; Vide   ; Val  6 ; Vide   ; Vide   |];
   [| Vide   ; Val 3  ; Vide   ; Vide   ; Val  8 ; Vide   ; Vide   ; Val  9 ; Vide   |];
   [| Vide   ; Vide   ; Val  7 ; Vide   ; Vide   ; Vide   ; Vide   ; Vide   ; Val  2 |]|];;
