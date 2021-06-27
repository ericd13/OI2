let carre_pe_pos m p i =
  let n = Array.length m in
  let j = ref 0 in
    while !j < p && i + !j + p  < n 
          && m.(i + !j + p) = m.(i + !j)
    do j := !j + 1 done;
    !j;;

let carre_pe m p =
  let n = Array.length m in
  let rec aux i =
    if i = n
    then false
    else if carre_pe_pos m p i = p
    then true
    else aux (i+1)
  in aux 0;;

let carre_naif m =
  let n = Array.length m in
  let r = n/2 in
  let rec aux p =
    if p = r+1
    then false
    else if carre_pe m p
    then true
    else aux (p+1)
  in aux 1;;


carre_naif [|'c'; 'o'; 'c'; 'o'; 'o'; 'a' |];;
